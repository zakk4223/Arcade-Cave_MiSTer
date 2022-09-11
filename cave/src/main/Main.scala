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

package cave.main

import arcadia._
import arcadia.cpu.m68k._
import arcadia.gfx._
import arcadia.mem._
import arcadia.mister._
import cave._
import cave.gfx._
import cave.snd.SoundCtrlIO
import chisel3._
import chisel3.util._

/** Represents the main PCB. */
class Main extends Module {
  val io = IO(new Bundle {
    /** Video clock domain */
    val videoClock = Input(Clock())
    /** Game config port */
    val gameConfig = Input(GameConfig())
    /** Options port */
    val options = OptionsIO()
    /** Joystick port */
    val player = Vec(2, PlayerIO())
    /** DIP switches port */
    val dips = DIPIO()
    /** Video port */
    val video = Flipped(VideoIO())
    /** Sound control port */
    val soundCtrl = Flipped(SoundCtrlIO())
    /** Program ROM port */
    val progRom = new ProgRomIO
    /** EEPROM port */
    val eeprom = new EEPROMIO
    /** Layer tile ROM port */
    val layerTileRom = Vec(Config.LAYER_COUNT, new TileRomIO)
    /** Sprite tile ROM port */
    val spriteTileRom = new SpriteRomIO
    /** Sprite line buffer port */
    val spriteLineBuffer = new SpriteLineBufferIO
    /** Sprite frame buffer port */
    val spriteFrameBuffer = new SpriteFrameBufferIO
    /** System frame buffer port */
    val systemFrameBuffer = new SystemFrameBufferIO
    /** Asserted when the current page for the sprite frame buffer should be swapped */
    val spriteFrameBufferSwap = Output(Bool())
    /** RGB port */
    val rgb = Output(UInt(Config.RGB_WIDTH.W))
  })

  // A write-only memory interface is used to connect the CPU to the EEPROM
  val eepromMem = Wire(WriteMemIO(CPU.ADDR_WIDTH, CPU.DATA_WIDTH))

  // Synchronize vertical blank into system clock domain
  val vBlank = ShiftRegister(io.video.vBlank, 2)
  val vBlankRising = Util.rising(vBlank)
  val vBlankFalling = Util.falling(vBlank)

  // Toggle pause register
  val pauseReg = Util.toggle(Util.rising(io.player(0).pause || io.player(1).pause))

  // IRQ signals
  val videoIrq = RegInit(false.B)
  val agalletIrq = RegInit(false.B)
  val unknownIrq = RegInit(false.B)

  // M68K CPU
  val cpu = Module(new CPU(Config.CPU_CLOCK_DIV))
  val map = new MemMap(cpu.io)
  cpu.io.halt := pauseReg
  cpu.io.dtack := false.B
  cpu.io.vpa := cpu.io.as && cpu.io.fc === 7.U // autovectored interrupts
  cpu.io.ipl := videoIrq || io.soundCtrl.irq || unknownIrq
  cpu.io.din := 0.U

  // Graphics processor
  val gpu = Module(new GPU)
  gpu.io.videoClock := io.videoClock
  gpu.io.gameConfig <> io.gameConfig
  gpu.io.options <> io.options
  gpu.io.video <> io.video
  0.until(Config.LAYER_COUNT).foreach { i =>
    gpu.io.layerCtrl(i).format := io.gameConfig.layer(i).format
    gpu.io.layerCtrl(i).enable := io.options.layer(i)
    gpu.io.layerCtrl(i).tileRom <> Crossing.syncronize(io.videoClock, io.layerTileRom(i))
  }
  gpu.io.spriteCtrl.format := io.gameConfig.sprite.format
  gpu.io.spriteCtrl.enable := io.options.sprite
  gpu.io.spriteCtrl.start := vBlankFalling
  gpu.io.spriteCtrl.zoom := io.gameConfig.sprite.zoom
  gpu.io.spriteCtrl.tileRom <> io.spriteTileRom
  gpu.io.spriteLineBuffer <> io.spriteLineBuffer
  gpu.io.spriteFrameBuffer <> io.spriteFrameBuffer
  gpu.io.systemFrameBuffer <> io.systemFrameBuffer
  io.rgb := gpu.io.rgb

  // Set interface defaults
  io.soundCtrl.oki(0).default()
  io.soundCtrl.oki(1).default()
  io.soundCtrl.nmk.default()
  io.soundCtrl.ymz.default()
  io.progRom.default()

  // EEPROM
  val eeprom = Module(new EEPROM)
  eeprom.io.mem <> io.eeprom
  val cs = Mux(io.gameConfig.index === GameConfig.GUWANGE.U, eepromMem.din(5), eepromMem.din(9))
  val sck = Mux(io.gameConfig.index === GameConfig.GUWANGE.U, eepromMem.din(6), eepromMem.din(10))
  val sdi = Mux(io.gameConfig.index === GameConfig.GUWANGE.U, eepromMem.din(7), eepromMem.din(11))
  eeprom.io.serial.cs := RegEnable(cs, false.B, eepromMem.wr)
  eeprom.io.serial.sck := RegEnable(sck, false.B, eepromMem.wr)
  eeprom.io.serial.sdi := RegEnable(sdi, false.B, eepromMem.wr)
  eepromMem.default()

  // Main RAM
  val mainRam = Module(new SinglePortRam(
    addrWidth = Config.MAIN_RAM_ADDR_WIDTH,
    dataWidth = Config.MAIN_RAM_DATA_WIDTH,
    maskEnable = true
  ))
  mainRam.io.default()

  // Sprite VRAM
  val spriteRam = Module(new TrueDualPortRam(
    addrWidthA = Config.SPRITE_RAM_ADDR_WIDTH,
    dataWidthA = Config.SPRITE_RAM_DATA_WIDTH,
    addrWidthB = Config.SPRITE_RAM_GPU_ADDR_WIDTH,
    dataWidthB = Config.SPRITE_RAM_GPU_DATA_WIDTH,
    maskEnable = true
  ))
  spriteRam.io.clockB := clock // system (i.e. fast) clock domain
  spriteRam.io.portA.default()
  spriteRam.io.portB <> gpu.io.spriteCtrl.vram

  // Layer VRAM (8x8)
  val vram8x8 = 0.until(Config.LAYER_COUNT).map { i =>
    val ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LAYER_8x8_RAM_ADDR_WIDTH,
      dataWidthA = Config.LAYER_RAM_DATA_WIDTH,
      addrWidthB = Config.LAYER_8x8_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LAYER_RAM_GPU_DATA_WIDTH,
      maskEnable = true
    ))
    ram.io.clockB := io.videoClock
    ram.io.portA.default()
    ram.io.portB <> gpu.io.layerCtrl(i).vram8x8
    ram
  }

  // Layer VRAM (16x16)
  val vram16x16 = 0.until(Config.LAYER_COUNT).map { i =>
    val ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LAYER_16x16_RAM_ADDR_WIDTH,
      dataWidthA = Config.LAYER_RAM_DATA_WIDTH,
      addrWidthB = Config.LAYER_16x16_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LAYER_RAM_GPU_DATA_WIDTH,
      maskEnable = true
    ))
    ram.io.clockB := io.videoClock
    ram.io.portA.default()
    ram.io.portB <> gpu.io.layerCtrl(i).vram16x16
    ram
  }

  // Line RAM
  val lineRam = 0.until(Config.LAYER_COUNT).map { i =>
    val ram = Module(new TrueDualPortRam(
      addrWidthA = Config.LINE_RAM_ADDR_WIDTH,
      dataWidthA = Config.LINE_RAM_DATA_WIDTH,
      addrWidthB = Config.LINE_RAM_GPU_ADDR_WIDTH,
      dataWidthB = Config.LINE_RAM_GPU_DATA_WIDTH,
      maskEnable = true
    ))
    ram.io.clockB := io.videoClock
    ram.io.portA.default()
    ram.io.portB <> gpu.io.layerCtrl(i).lineRam
    ram
  }

  // Palette RAM
  val paletteRam = Module(new TrueDualPortRam(
    addrWidthA = Config.PALETTE_RAM_ADDR_WIDTH,
    dataWidthA = Config.PALETTE_RAM_DATA_WIDTH,
    addrWidthB = Config.PALETTE_RAM_GPU_ADDR_WIDTH,
    dataWidthB = Config.PALETTE_RAM_GPU_DATA_WIDTH,
    maskEnable = true
  ))
  paletteRam.io.clockB := io.videoClock
  paletteRam.io.portA.default()
  paletteRam.io.portB <> gpu.io.paletteRam

  // Layer registers
  val layerRegs = 0.until(Config.LAYER_COUNT).map { i =>
    val regs = Module(new RegisterFile(CPU.DATA_WIDTH, Config.LAYER_REGS_COUNT))
    regs.io.mem.default()
    gpu.io.layerCtrl(i).regs := withClock(io.videoClock) { ShiftRegister(LayerRegs.decode(regs.io.regs), 2) }
    regs
  }

  // Sprite registers
  val spriteRegs = Module(new RegisterFile(CPU.DATA_WIDTH, Config.SPRITE_REGS_COUNT))
  spriteRegs.io.mem.default()
  gpu.io.spriteCtrl.regs := SpriteRegs.decode(spriteRegs.io.regs)

  // Toggle video IRQ
  when(vBlankRising) {
    videoIrq := true.B
    agalletIrq := true.B
  }.elsewhen(Util.falling(vBlank)) {
    agalletIrq := false.B
  }

  // IRQ cause handler
  val irqCause = { (_: UInt, offset: UInt) =>
    val a = offset === 0.U && agalletIrq
    val b = unknownIrq
    val c = videoIrq
    when(offset === 4.U) { videoIrq := false.B }
    when(offset === 6.U) { unknownIrq := false.B }
    Cat(!a, !b, !c)
  }

  // Set player input ports
  val (input0, input1) = Main.encodePlayers(io.gameConfig.index, io.options, io.player, eeprom)

  // Swap the sprite frame buffer while the CPU is paused. This allows the GPU to continue rendering
  // sprites.
  io.spriteFrameBufferSwap := pauseReg && vBlankRising

  /**
   * Maps video RAM to the given base address.
   *
   * @param baseAddr  The base memory address.
   * @param vram8x8   The 8x8 VRAM memory interface.
   * @param vram16x16 The 16x16 VRAM memory interface.
   * @param lineRam   The line RAM memory interface.
   */
  def vramMap(baseAddr: Int, vram8x8: MemIO, vram16x16: MemIO, lineRam: MemIO): Unit = {
    map((baseAddr + 0x0000) to (baseAddr + 0x0fff)).readWriteMem(vram16x16)
    map((baseAddr + 0x1000) to (baseAddr + 0x17ff)).readWriteMem(lineRam)
    map((baseAddr + 0x1800) to (baseAddr + 0x3fff)).readWriteStub()
    map((baseAddr + 0x4000) to (baseAddr + 0x7fff)).readWriteMem(vram8x8)
    map((baseAddr + 0x8000) to (baseAddr + 0xffff)).readWriteStub()
  }

  /**
   * Maps video registers to the given base address.
   *
   * @param baseAddr The base memory address.
   */
  def vregMap(baseAddr: Int): Unit = {
    map((baseAddr + 0x00) to (baseAddr + 0x07)).r(irqCause)
    map((baseAddr + 0x00) to (baseAddr + 0x0f)).writeMem(spriteRegs.io.mem.asWriteMemIO)
    map(baseAddr + 0x08).w { (_, _, _) => io.spriteFrameBufferSwap := true.B }
    map((baseAddr + 0x0a) to (baseAddr + 0x7f)).noprw()
  }

  // Access to 0x11xxxx appears during the service menu. It must be ignored, otherwise the service
  // menu freezes.
  map(0x110000 to 0x1fffff).noprw()

  // Dangun Feveron
  when(io.gameConfig.index === GameConfig.DFEVERON.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    map(0x300000 to 0x300003).readWriteMem(io.soundCtrl.ymz)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    vramMap(0x600000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    map(0x708000 to 0x708fff).readWriteMemT(paletteRam.io.portA)(a => a(10, 0))
    map(0x710c12 to 0x710c1f).noprw() // unused
    vregMap(0x800000)
    map(0x900000 to 0x900005).readWriteMem(layerRegs(0).io.mem)
    map(0xa00000 to 0xa00005).readWriteMem(layerRegs(1).io.mem)
    map(0xb00000).r { (_, _) => input0 }
    map(0xb00002).r { (_, _) => input1 }
    map(0xc00000).writeMem(eepromMem)
  }

  // DonPachi
  when(io.gameConfig.index === GameConfig.DONPACHI.U) {
    map(0x000000 to 0x07ffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    vramMap(0x200000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    vramMap(0x300000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    map(0x400000 to 0x40ffff).readWriteMemT(vram8x8(2).io.portA)(a => a(12, 0)) // layer 2 is 8x8 only
    map(0x500000 to 0x50ffff).readWriteMem(spriteRam.io.portA)
    map(0x600000 to 0x600005).readWriteMem(layerRegs(1).io.mem)
    map(0x700000 to 0x700005).readWriteMem(layerRegs(0).io.mem)
    map(0x800000 to 0x800005).readWriteMem(layerRegs(2).io.mem)
    vregMap(0x900000)
    map(0xa08000 to 0xa08fff).readWriteMemT(paletteRam.io.portA)(a => a(10, 0))
    map(0xb00000 to 0xb00003).readWriteMem(io.soundCtrl.oki(0))
    map(0xb00010 to 0xb00013).readWriteMem(io.soundCtrl.oki(1))
    map(0xb00020 to 0xb0002f).writeMem(io.soundCtrl.nmk)
    map(0xc00000).r { (_, _) => input0 }
    map(0xc00002).r { (_, _) => input1 }
    map(0xd00000).writeMem(eepromMem)
  }

  // DoDonPachi
  when(io.gameConfig.index === GameConfig.DDONPACH.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    map(0x300000 to 0x300003).readWriteMem(io.soundCtrl.ymz)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    map(0x5fff00 to 0x5fffff).nopw() // access occurs during attract loop
    vramMap(0x600000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    map(0x700000 to 0x70ffff).readWriteMemT(vram8x8(2).io.portA)(a => a(12, 0)) // layer 2 is 8x8 only
    vregMap(0x800000)
    map(0x900000 to 0x900005).readWriteMem(layerRegs(0).io.mem)
    map(0xa00000 to 0xa00005).readWriteMem(layerRegs(1).io.mem)
    map(0xb00000 to 0xb00005).readWriteMem(layerRegs(2).io.mem)
    map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
    map(0xd00000).r { (_, _) => input0 }
    map(0xd00002).r { (_, _) => input1 }
    map(0xe00000).writeMem(eepromMem)
  }

  // ESP Ra.De.
  when(io.gameConfig.index === GameConfig.ESPRADE.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    map(0x300000 to 0x300003).readWriteMem(io.soundCtrl.ymz)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    vramMap(0x600000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    vramMap(0x700000, vram8x8(2).io.portA, vram16x16(2).io.portA, lineRam(2).io.portA)
    vregMap(0x800000)
    map(0x800f00 to 0x800f03).nopr() // access occurs during attract loop
    map(0x900000 to 0x900005).readWriteMem(layerRegs(0).io.mem)
    map(0xa00000 to 0xa00005).readWriteMem(layerRegs(1).io.mem)
    map(0xb00000 to 0xb00005).readWriteMem(layerRegs(2).io.mem)
    map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
    map(0xd00000).r { (_, _) => input0 }
    map(0xd00002).r { (_, _) => input1 }
    map(0xe00000).writeMem(eepromMem)
  }

  // Gaia Crusaders
  when(io.gameConfig.index === GameConfig.GAIA.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x00057e to 0x000581).nopw() // access occurs during boot
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    map(0x300000 to 0x300003).readWriteMem(io.soundCtrl.ymz)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    vramMap(0x600000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    vramMap(0x700000, vram8x8(2).io.portA, vram16x16(2).io.portA, lineRam(2).io.portA)
    vregMap(0x800000)
    map(0x900000 to 0x900005).readWriteMem(layerRegs(0).io.mem)
    map(0xa00000 to 0xa00005).readWriteMem(layerRegs(1).io.mem)
    map(0xb00000 to 0xb00005).readWriteMem(layerRegs(2).io.mem)
    map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
    map(0xd00010).r { (_, _) => input0 }
    map(0xd00010).nopw() // coin counter
    map(0xd00012).r { (_, _) => input1 }
    map(0xd00014).r { (_, _) => io.dips(0) }
    map(0xd00014).nopw() // watchdog
  }

  // Guwange
  when(io.gameConfig.index === GameConfig.GUWANGE.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x200000 to 0x20ffff).readWriteMem(mainRam.io)
    map(0x210000 to 0x2fffff).nopr() // access occurs for Guwange (Special)
    vregMap(0x300000)
    map(0x300080 to 0x3fffff).nopr() // access occurs for Guwange (Special)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    map(0x410000 to 0x4fffff).nopr() // access occurs for Guwange (Special)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    map(0x508000 to 0x5fffff).nopr() // access occurs for Guwange (Special)
    vramMap(0x600000, vram8x8(1).io.portA, vram16x16(1).io.portA, lineRam(1).io.portA)
    map(0x608000 to 0x6fffff).nopr() // access occurs for Guwange (Special)
    vramMap(0x700000, vram8x8(2).io.portA, vram16x16(2).io.portA, lineRam(2).io.portA)
    map(0x800000 to 0x800003).readWriteMem(io.soundCtrl.ymz)
    map(0x900000 to 0x900005).readWriteMem(layerRegs(0).io.mem)
    map(0xa00000 to 0xa00005).readWriteMem(layerRegs(1).io.mem)
    map(0xb00000 to 0xb00005).readWriteMem(layerRegs(2).io.mem)
    map(0xc00000 to 0xc0ffff).readWriteMem(paletteRam.io.portA)
    map(0xd00010 to 0xd00014).noprw()
    map(0xd00010).writeMem(eepromMem)
    map(0xd00010).r { (_, _) => input0 }
    map(0xd00012).r { (_, _) => input1 }
  }

  // Puzzle Uo Poko
  when(io.gameConfig.index === GameConfig.UOPOKO.U) {
    map(0x000000 to 0x0fffff).readMemT(io.progRom) { _ ## 0.U } // convert to byte address
    map(0x100000 to 0x10ffff).readWriteMem(mainRam.io)
    map(0x300000 to 0x300003).readWriteMem(io.soundCtrl.ymz)
    map(0x400000 to 0x40ffff).readWriteMem(spriteRam.io.portA)
    vramMap(0x500000, vram8x8(0).io.portA, vram16x16(0).io.portA, lineRam(0).io.portA)
    vregMap(0x600000)
    map(0x700000 to 0x700005).readWriteMem(layerRegs(0).io.mem)
    map(0x800000 to 0x80ffff).readWriteMem(paletteRam.io.portA)
    map(0x900000).r { (_, _) => input0 }
    map(0x900002).r { (_, _) => input1 }
    map(0xa00000).writeMem(eepromMem)
  }
}

object Main {
  /**
   * Encodes the player inputs and EEPROM data.
   *
   * @param gameIndex The game index.
   * @param options   The options port.
   * @param player    The player port.
   * @param eeprom    The eeprom port.
   * @return A tuple representing the left and right player inputs.
   */
  private def encodePlayers(gameIndex: UInt, options: OptionsIO, player: Vec[PlayerIO], eeprom: EEPROM): (UInt, UInt) = {
    // Trigger coin pulse
    val coin1 = Util.pulseSync(Config.COIN_PULSE_WIDTH, player(0).coin)
    val coin2 = Util.pulseSync(Config.COIN_PULSE_WIDTH, player(1).coin)

    // Trigger service button press
    val service = Util.pulseSync(Config.SERVICE_PULSE_WIDTH, options.service)

    val default1 = Cat("b111111".U, ~service, ~coin1, ~player(0).start, ~player(0).buttons(2, 0), ~player(0).right, ~player(0).left, ~player(0).down, ~player(0).up)
    val default2 = Cat("b1111".U, eeprom.io.serial.sdo, "b11".U, ~coin2, ~player(1).start, ~player(1).buttons(2, 0), ~player(1).right, ~player(1).left, ~player(1).down, ~player(1).up)

    val left = MuxLookup(gameIndex, default1, Seq(
      GameConfig.GAIA.U -> Cat(~player(1).buttons(3, 0), ~player(1).right, ~player(1).left, ~player(1).down, ~player(1).up, ~player(0).buttons(3, 0), ~player(0).right, ~player(0).left, ~player(0).down, ~player(0).up),
      GameConfig.GUWANGE.U -> Cat(~player(1).buttons(2, 0), ~player(1).right, ~player(1).left, ~player(1).down, ~player(1).up, ~player(1).start, ~player(0).buttons(2, 0), ~player(0).right, ~player(0).left, ~player(0).down, ~player(0).up, ~player(0).start),
    ))

    val right = MuxLookup(gameIndex, default2, Seq(
      GameConfig.GAIA.U -> Cat("b0000111111".U, ~player(1).start, ~player(0).start, "b1".U, ~service, ~coin2, ~coin1),
      GameConfig.GUWANGE.U -> Cat("b11111111".U, eeprom.io.serial.sdo, "b1111".U, ~service, ~coin2, ~coin1),
    ))

    (left, right)
  }
}
