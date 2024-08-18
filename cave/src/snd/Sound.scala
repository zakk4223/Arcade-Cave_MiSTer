/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (c) 2022 Josh Bassett
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package cave.snd

import arcadia.util.Counter
import arcadia.cpu.z80._
import arcadia.mem._
import arcadia.mem.arbiter.AsyncReadMemArbiter
import arcadia.snd._
import arcadia.clk.ClockDivider
import cave._
import chisel3._
import chisel3.util._

/**
 * Represents the sound PCB.
 *
 * Earlier games tend to use one (or more) OKIM6295 ADPCM decoder chips, while later games rely on
 * a single YMZ280B ADPCM decoder chip.
 */


class OkiBanks extends Bundle {
  val bankHi = UInt(4.W)
  val bankLo = UInt(4.W)
}

class Sound extends Module {
  val io = IO(new Bundle {
    /** Control port */
    val ctrl = SoundCtrlIO()
    /** Game index */
    val gameIndex = Input(UInt(4.W))
    /** Game config port */
    val gameConfig = Input(GameConfig())
    /** Sound ROM port */
    val rom = Vec(Config.SOUND_ROM_COUNT, new SoundRomIO)
    /** Audio port */
    val audio = Output(SInt(Config.AUDIO_SAMPLE_WIDTH.W))
  })

  // Wires
  val irq = Wire(Bool())

  // Registers
  val reqReg = RegEnable(true.B, false.B, io.ctrl.req)

  val dataReg = RegEnable(io.ctrl.data, io.ctrl.req)
  val z80BankReg = RegInit(0.U(5.W))
  val mainLatchReg = RegInit(0.U(8.W))

  // Sound CPU
  val cpu = Module(new CPU())
  val memMap = new MemMap(cpu.io)
  val ioMap = new IOMap(cpu.io, 0xff)
  cpu.io.halt := false.B
  cpu.io.din := DontCare
  cpu.io.int := irq
  cpu.io.nmi := reqReg
  io.ctrl.ack := false.B
  io.ctrl.ackData := 0.U(8.W)

  when(io.gameIndex === Game.AGALLET.U) {
    val (_, cen) = Counter.static(4)
    cpu.io.cen := cen
  }.otherwise {
    val (_, cen) = Counter.static(8)
    cpu.io.cen := cen
  }

  // Sound RAM
  val soundRam = Module(new TrueDualPortRam(
    addrWidthA = Sound.SOUND_RAM_ADDR_WIDTH,
    dataWidthA = Sound.SOUND_RAM_DATA_WIDTH,
    addrWidthB = Sound.SOUND_RAM_ADDR_WIDTH,
    dataWidthB = Sound.SOUND_RAM_DATA_WIDTH
  ))
  soundRam.io.clockB := clock 
  soundRam.io.portA.default()
  soundRam.io.portB.default()

  // NMK112 banking controller
  // 
  val nmk = Module(new NMK112)
  nmk.io.cpu <> io.ctrl.nmk
  nmk.io.mask := 1.U // disable phrase table bank switching for chip 0 (background music)

  // OKIM6295 ADPCM decoder

  val oki = 0.until(Sound.OKI_COUNT).map { i =>
    val oki = Module(new OKIM6295(Config.okiConfig(i)))
    oki.io.cpu <> io.ctrl.oki(i)
    oki
  }

  when (io.gameIndex === Game.AGALLET.U)  {
    oki(0).io.cen := ClockDivider(Config.CPU_CLOCK_FREQ / 2_112_000)
    oki(1).io.cen := ClockDivider(Config.CPU_CLOCK_FREQ / 2_112_000)
  }.otherwise {
    oki(0).io.cen := ClockDivider(Config.CPU_CLOCK_FREQ / 1_056_000)
    oki(1).io.cen := ClockDivider(Config.CPU_CLOCK_FREQ / 2_112_000)
  }

  val okiBank = 0.until(Sound.OKI_COUNT).map { i =>
    val okiBank = RegInit(0.U.asTypeOf(new OkiBanks())) 
    okiBank
  } 

  // YMZ280B ADPCM decoder
  val ymz280b = Module(new YMZ280B(Config.ymzConfig))
  ymz280b.io.cpu <> io.ctrl.ymz
  ymz280b.io.rom <> io.rom(0)
  io.ctrl.irq := ymz280b.io.irq

  // YM2203 FM synthesizer
  val ym2203 = Module(new YM2203(clockFreq = Config.CPU_CLOCK_FREQ, sampleFreq = Sound.FM_SAMPLE_CLOCK_FREQ))
  ym2203.io.cpu.default()

  val ym2151 = Module(new YM2151(clockFreq = Config.CPU_CLOCK_FREQ, sampleFreq = Sound.FM_SAMPLE_CLOCK_FREQ))
  ym2151.io.cpu.default()
  irq := ym2151.io.irq


  // Program and bank ROM wires
  val progRom = Wire(AsyncReadMemIO(Config.SOUND_ROM_ADDR_WIDTH, Config.SOUND_ROM_DATA_WIDTH))
  val bankRom = Wire(AsyncReadMemIO(Config.SOUND_ROM_ADDR_WIDTH, Config.SOUND_ROM_DATA_WIDTH))
  progRom.default()
  bankRom.default()

  // Connect sound ROM port 0
  //
  val arbiter = Module(new AsyncReadMemArbiter(4, Config.SOUND_ROM_ADDR_WIDTH, Config.SOUND_ROM_DATA_WIDTH))
  arbiter.connect(
    oki(0).io.rom.mapAddr(mapOkiAddr(0)).enable(io.gameConfig.sound(0).device === SoundDevice.OKIM6259.U),
    ymz280b.io.rom.enable(io.gameConfig.sound(0).device === SoundDevice.YMZ280B.U),
    progRom.enable(io.gameConfig.sound(0).device === SoundDevice.Z80.U),
    bankRom.enable(io.gameConfig.sound(0).device === SoundDevice.Z80.U),
  ) <> io.rom(0)

when(io.gameConfig.sound(2).device === SoundDevice.OKIM6259.U) {
  oki(0).io.rom.mapAddr(mapOkiAddr(0)) <> io.rom(1)
  oki(1).io.rom.mapAddr(mapOkiAddr(1)) <> io.rom(2)
}.otherwise {
  oki(1).io.rom.mapAddr(mapOkiAddr(1)) <> io.rom(1)
  io.rom(2) <> DontCare
}


  /**
   * Gets the latch value and clears the request register.
   *
   * @param high The high byte flag.
   */
  def getLatch(high: Boolean): Bits = {
    reqReg := false.B
    if (high) dataReg(15, 8) else dataReg(7, 0)
  }

  def setAckLatchData(data: Bits): Unit = {
      io.ctrl.ack := true.B
      io.ctrl.ackData := data
  }


  /**
   * Maps the given OKI address using the sound banking configuration.
   *
   * @param addr The memory address.
   */
  def mapOkiAddr(chip: Int)(addr: UInt): UInt = {
    val bank = Mux(addr(17), okiBank(chip).bankHi, okiBank(chip).bankLo)
    bank ## addr(16, 0)
    Mux(io.gameIndex === Game.DONPACHI.U,
      nmk.transform(chip)(addr),
      bank ## addr(16, 0)
    )
  }

  /**
   * Sets the OKI bank registers.
   *
   * @param mask The bank mask value.
   * @param data The data to be written to the bank registers.
   */
  def setOkiBank(chip: Int, mask: Int, data: Bits): Unit = {
    okiBank(chip).bankHi := data(7, 4) & mask.U
    okiBank(chip).bankLo := data(3, 0) & mask.U
  }

  // Hotdog Storm
  when(io.gameIndex === Game.HOTDOGST.U) {
    memMap(0x0000 to 0x3fff).readMem(progRom)
    memMap(0x4000 to 0x7fff).readMemT(bankRom) { addr => z80BankReg ## addr(13, 0) }
    memMap(0xe000 to 0xffff).readWriteMem(soundRam.io.portA)
    memMap(0xd000 to 0xdfff).readWriteStub()

    ioMap(0x00).w { (_, _, data) => z80BankReg := data(3, 0) }
    ioMap(0x30).r { (_, _) => getLatch(false) }
    ioMap(0x40).r { (_, _) => getLatch(true) }
    ioMap(0x50 to 0x51).readWriteMem(ym2203.io.cpu) //ym2203_device
    ioMap(0x60).readWriteMem(oki(1).io.cpu)
    ioMap(0x70).w { (_, _, data) => setOkiBank(1, 0x3, data) }
  }.elsewhen (io.gameIndex === Game.AGALLET.U) {
    memMap(0x0000 to 0x3fff).readMem(progRom)
    memMap(0x4000 to 0x7fff).readMemT(bankRom) { addr => z80BankReg ## addr(13, 0) }
    memMap(0x8000 to 0xbfff).readWriteStub()
    memMap(0xc000 to 0xdfff).readWriteMem(soundRam.io.portA)
    memMap(0xe000 to 0xFFFF).readMem(soundRam.io.portB)
    ioMap(0x00).w { (_, _, data) => z80BankReg := data(4, 0)}
    ioMap(0x10).w { (_, _, data) => setAckLatchData(data)}
    ioMap(0x20).nopr() 
    ioMap(0x30).r { (_, _) => getLatch(false) }
    ioMap(0x40).r { (_, _) => getLatch(true) }
    ioMap(0x50 to 0x51).readWriteMem(ym2151.io.cpu) 
    ioMap(0x60).readWriteMem(oki(0).io.cpu)
    ioMap(0x6f).nopr()
    ioMap(0x70).w { (_, _, data) => setOkiBank(0, 0xf, data) }
    ioMap(0x80).readWriteMem(oki(1).io.cpu)
    ioMap(0xc0).w { (_, _, data) => setOkiBank(1, 0xf, data) }
  }

  // Audio mixer
  io.audio := AudioMixer.sum(Config.AUDIO_SAMPLE_WIDTH,
    RegEnable(ymz280b.io.audio.bits.left, ymz280b.io.audio.valid) -> 1.0,
    RegEnable(ym2203.io.audio.bits.psg, ym2203.io.audio.valid) -> 1.0,
    RegEnable(ym2203.io.audio.bits.fm, ym2203.io.audio.valid) -> 1.0,
    RegEnable(ym2151.io.audio.bits.left, ym2151.io.audio.valid) -> 1.0,
    RegEnable(ym2151.io.audio.bits.right, ym2151.io.audio.valid) -> 1.0,
    RegEnable(oki(0).io.audio.bits, oki(0).io.audio.valid) -> 1.6,
    RegEnable(oki(1).io.audio.bits, oki(1).io.audio.valid) -> 1.0
  )
}

object Sound {
  /** The FM sample clock frequency (Hz) */
  val FM_SAMPLE_CLOCK_FREQ = 4_000_000
  /** The width of the sound RAM address bus */
  val SOUND_RAM_ADDR_WIDTH = 13
  /** The width of the sound RAM data bus */
  val SOUND_RAM_DATA_WIDTH = 8
  /** The number of OKIM6295 chips */
  val OKI_COUNT = 2
}
