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

package arcadia.mem.sdram
import arcadia.mem.BurstMemIO
import arcadia.mem.request.Request
import arcadia.util.Counter
import chisel3._
import chisel3.util._
import chisel3.experimental.annotate

/**
 * Handles reading/writing data to a SDRAM memory device.
 *
 * @param config The SDRAM configuration.
 */
class SDRAM(config: Config) extends Module {
  // Sanity check
  assert(Seq(1, 2, 4, 8).contains(config.burstLength), "SDRAM burst length must be 1, 2, 4, or 8")

  val io = IO(new Bundle {
    /** Memory port */
    val mem = Flipped(BurstMemIO(config))
    /** Device port */
    val sdram = SDRAMIO(config)
    /** Debug port */
    val debug = Output(new Bundle {
      val init = Bool()
      val mode = Bool()
      val idle = Bool()
      val active = Bool()
      val read = Bool()
      val write = Bool()
      val refresh = Bool()
    })
  })

  // States
  object State {
    val init :: mode :: idle :: active :: read :: write :: refresh :: Nil = Enum(7)
  }

  // Commands
  object Command {
    val mode :: refresh :: precharge :: active :: write :: read :: stop :: nop :: deselect :: Nil = Enum(9)
  }

  class HotState extends Bundle {
    val init = Bool()
    val mode = Bool()
    val idle = Bool()
    val active = Bool()
    val read = Bool()
    val write = Bool()
    val refresh = Bool()

    def =/=(other: HotState): Bool = {
      this.init =/= other.init || this.mode =/= other.mode ||
      this.idle =/= other.idle || this.active =/= other.active ||
      this.read =/= other.read || this.write =/= other.write ||
      this.refresh =/= other.refresh
    }
  }

  val nextState = Wire(new HotState)
  val stateReg = RegNext(nextState, {
    val s = Wire(new HotState)
    s.init := true.B
    s.mode := false.B
    s.idle := false.B
    s.active := false.B
    s.read := false.B
    s.write := false.B
    s.refresh := false.B
    s
  })
  nextState := stateReg

  // State register
  //val nextState = Wire(UInt())
  //val stateReg = RegNext(nextState, State.init)

  // Command register
  val nextCommand = Wire(UInt())
  val commandReg = RegNext(nextCommand, Command.nop)

  // Assert the latch signal when a request should be latched
  val latch = !stateReg.active && nextState.active

  // Asserted when there is a read or write request
  val isReadWrite = io.mem.rd || io.mem.wr

  // Request register
  val request = Request(io.mem.rd, io.mem.wr, Address.fromByteAddress(config, io.mem.addr), 0.U, 0.U)
  val requestReg = RegEnable(request, latch)

  // SDRAM registers
  //
  // Using simple registers for SDRAM I/O allows better timings to be achieved, because they can be
  // optimized and moved physically closer to the FPGA pins during routing (i.e. fast registers).
  val bankReg = Reg(UInt())
  val addrReg = Reg(UInt())
  val dinReg = RegNext(io.mem.din)
  val doutReg = RegNext(io.sdram.dout)

  // Counters
  val (waitCounter, _) = Counter.static(config.waitCounterMax, reset = nextState =/= stateReg)
  val (refreshCounter, _) = Counter.static(config.refreshCounterMax,
    enable = !stateReg.init && !stateReg.mode,
    reset = stateReg.refresh && waitCounter === 0.U
  )

  val waitCounter_dup =  WireDefault(waitCounter)
  val waitCounter_dup1 = WireDefault(waitCounter)
  val waitCounter_dup2 = WireDefault(waitCounter)
  val waitCounter_dup3 = WireDefault(waitCounter)
  val waitCounter_dup4 = WireDefault(waitCounter)
  val waitCounter_dup5 = WireDefault(waitCounter)
  val waitCounter_dup6 = WireDefault(waitCounter)
  val waitCounter_dup7 = WireDefault(waitCounter)

  dontTouch(waitCounter_dup)
  dontTouch(waitCounter_dup1)
  dontTouch(waitCounter_dup2)
  dontTouch(waitCounter_dup3)
  dontTouch(waitCounter_dup4)
  dontTouch(waitCounter_dup5)
  dontTouch(waitCounter_dup6)
  dontTouch(waitCounter_dup7)

  // Control signals
  val modeDone = waitCounter_dup1 === (config.modeWait - 1).U
  val activeDone = waitCounter_dup2 === (config.activeWait - 1).U
  val readDone = RegNext(waitCounter_dup === (config.readWait - 1).U, false.B)
  val writeDone = waitCounter_dup3 === (config.writeWait - 1).U
  val refreshDone = waitCounter_dup4 === (config.refreshWait - 1).U
  val triggerRefresh = refreshCounter >= (config.refreshInterval - 1).U
  val burstBusy = waitCounter_dup5 < (config.burstLength - 1).U
  val burstDone = waitCounter_dup6 === (config.burstLength - 1).U

  // Deassert the wait signal at the start of a read request, or during a write request
  val wait_n = {
    val idle = stateReg.idle && !isReadWrite
    val read = latch && request.rd
    val write = (stateReg.active && activeDone && requestReg.wr) || (stateReg.write && burstBusy)
    idle || read || write
  }

  // Assert the valid signal after the first word has been bursted during a read
  val validReg = RegNext(stateReg.read && waitCounter_dup > (config.casLatency - 1).U, false.B)

  // Assert the burst done signal when a read/write burst has completed
  val memBurstDone = {
    val readBurstDone = stateReg.read && readDone
    val writeBurstDone = stateReg.write && burstDone
    RegNext(readBurstDone, false.B) || writeBurstDone
  }

  // Default to the previous state
  nextState := stateReg

  // Default to a NOP
  nextCommand := Command.nop

  def mode() = {
    nextState.init := false.B
    nextState.mode := true.B
    nextState.idle := false.B
    nextState.active := false.B
    nextState.read := false.B
    nextState.write := false.B
    nextState.refresh := false.B
    nextCommand := Command.mode
    addrReg := config.opcode
  }

  def idle() = {
    nextState.init := false.B
    nextState.mode := false.B
    nextState.idle := true.B
    nextState.active := false.B
    nextState.read := false.B
    nextState.write := false.B
    nextState.refresh := false.B
  }

  def active() = {
    nextState.init := false.B
    nextState.mode := false.B
    nextState.idle := false.B
    nextState.active := true.B
    nextState.read := false.B
    nextState.write := false.B
    nextState.refresh := false.B
    nextCommand := Command.active
    bankReg := request.addr.bank
    addrReg := request.addr.row
  }

  def read() = {
    nextState.init := false.B
    nextState.mode := false.B
    nextState.idle := false.B
    nextState.active := false.B
    nextState.read := true.B
    nextState.write := false.B
    nextState.refresh := false.B
    nextCommand := Command.read
    bankReg := requestReg.addr.bank
    addrReg := "b001".U ## requestReg.addr.col.pad(10)
  }

  def write() = {
    nextState.init := false.B
    nextState.mode := false.B
    nextState.idle := false.B
    nextState.active := false.B
    nextState.read := false.B
    nextState.write := true.B
    nextState.refresh := false.B
    nextCommand := Command.write
    bankReg := requestReg.addr.bank
    addrReg := "b001".U ## requestReg.addr.col.pad(10)
  }

  def refresh() = {
    nextState.init := false.B
    nextState.mode := false.B
    nextState.idle := false.B
    nextState.active := false.B
    nextState.read := false.B
    nextState.write := false.B
    nextState.refresh := true.B
    nextCommand := Command.refresh
  }

  // FSM
    // Initialize device
    when(stateReg.init) {
      addrReg := "b0010000000000".U
      when(waitCounter_dup7 === 0.U) {
        nextCommand := Command.deselect
      }.elsewhen(waitCounter_dup7 === (config.deselectWait - 1).U) {
        nextCommand := Command.precharge
      }.elsewhen(waitCounter_dup7 === (config.deselectWait + config.prechargeWait - 1).U) {
        nextCommand := Command.refresh
      }.elsewhen(waitCounter_dup7 === (config.deselectWait + config.prechargeWait + config.refreshWait - 1).U) {
        nextCommand := Command.refresh
      }.elsewhen(waitCounter_dup7 === (config.deselectWait + config.prechargeWait + config.refreshWait + config.refreshWait - 1).U) {
        mode()
      }
    }.elsewhen(stateReg.mode) {
      when(modeDone) { idle() }
    }.elsewhen(stateReg.idle) {
    // Wait for request
      when(triggerRefresh) { refresh() }.elsewhen(isReadWrite) { active() }
    }.elsewhen(stateReg.active) { 
      when(activeDone) {
        when(requestReg.wr) { write() }.otherwise { read() }
      }
    }.elsewhen(stateReg.read) {
    // Execute read command
      when(readDone) {
        when(triggerRefresh) { refresh() }.elsewhen(isReadWrite) { active() }.otherwise { idle() }
      }
    }.elsewhen(stateReg.write) {
    // Execute write command
      when(writeDone) {
        when(triggerRefresh) { refresh() }.elsewhen(isReadWrite) { active() }.otherwise { idle() }
      }
    }.elsewhen(stateReg.refresh) {
    // Execute refresh command
      when(refreshDone) {
        when(isReadWrite) { active() }.otherwise { idle() }
      }
    }

  // Outputs
  io.mem.wait_n := wait_n
  io.mem.valid := validReg
  io.mem.burstDone := memBurstDone
  io.mem.dout := doutReg
  io.sdram.cke := true.B
  io.sdram.cs_n := commandReg(3)
  io.sdram.ras_n := commandReg(2)
  io.sdram.cas_n := commandReg(1)
  io.sdram.we_n := commandReg(0)
  io.sdram.oe_n := !stateReg.read
  io.sdram.bank := bankReg
  io.sdram.addr := addrReg
  io.sdram.din := dinReg
  io.debug.init := stateReg.init
  io.debug.mode := stateReg.mode
  io.debug.idle := stateReg.idle
  io.debug.active := stateReg.active
  io.debug.read := stateReg.read
  io.debug.write := stateReg.write
  io.debug.refresh := stateReg.refresh

  // Debug
  if (sys.env.get("DEBUG").contains("1")) {
    printf(p"SDRAM(state: $stateReg, nextState: $nextState, command: $commandReg, nextCommand: $nextCommand, bank: $bankReg, addr: $addrReg, waitCounter: $waitCounter, wait: $wait_n, valid: $validReg, burstDone: $memBurstDone)\n")
  }
}
