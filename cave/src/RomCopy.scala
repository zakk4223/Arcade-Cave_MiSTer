
package cave

import arcadia.mem._
import arcadia.mem.arbiter.BurstMemArbiter
import arcadia.util.Counter
import arcadia.Util
import cave._
import chisel3.util._
import chisel3._

class RomCopy(inaddrWidth: Int, indataWidth: Int, outaddrWidth: Int, outdataWidth: Int) extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val gameConfig = Input(GameConfig())

    /** memory port */
    val in = BurstReadMemIO(inaddrWidth, indataWidth)
    val out = BurstWriteMemIO(outaddrWidth, outdataWidth)
  })

  /** I'm going to cheat here. I know this is a ddr->sdram copy, so I'm going to
   *  hardcode some widths/register sizes etc. 
   */

  val readOffset = RegInit(UInt(16.W), 0.U)
  val writeOffset = RegInit(UInt(32.W), 0.U)
  val writeCount = RegInit(UInt(16.W), 0.U) 

  val readReg = Reg(UInt(16.W))



  val stateIdle = RegInit(true.B)
  val stateRead = RegInit(false.B)
  val stateReadWait = RegInit(false.B)
  val stateReadValid = RegInit(false.B)
  val stateWrite = RegInit(false.B)
  val stateWriteDone = RegInit(false.B)
  val stateDone = RegInit(false.B)

  val write = RegInit(false.B)
  val read = RegInit(false.B)

  val readAddr = {
    writeOffset + readOffset 
  }

  val writeAddr = {
     writeOffset
  }

  val resultOffset = readAddr(2,0)


  when(stateIdle && io.start) {
    stateIdle := false.B
    stateRead := true.B
  }.elsewhen(stateRead) {
    when (io.in.valid) {
      stateRead := false.B
      stateReadValid := true.B
    }.elsewhen(io.in.wait_n) {
      stateRead := false.B
      stateReadWait := true.B
    }
  }.elsewhen(stateReadWait && io.in.valid && io.in.burstDone) {
    stateReadWait := false.B
    stateReadValid := true.B
  }.elsewhen(stateReadValid) {
    stateReadValid := false.B
    readReg := (io.in.dout >> (resultOffset << 3))(15,0) 
    readOffset := readOffset + 2.U
    stateWrite := true.B
  }.elsewhen(stateWrite && io.out.wait_n) {
    stateWrite := false.B
    stateWriteDone := true.B
  }.elsewhen(stateWriteDone) {
     writeOffset := writeOffset + 2.U
     stateWriteDone := false.B
     when (writeOffset + 2.U < (io.gameConfig.sprite.romOffset/2.U)) {
       stateIdle := true.B 
       readOffset := 0.U
     }.otherwise {
       stateDone := true.B
     }
  } /** Done state doesn't need to do anything... */

  io.in.addr := readAddr & (~7.U(64.W))
  io.out.addr := writeAddr
  io.in.burstLength := 1.U
  io.out.burstLength := 1.U
  io.done := stateDone
  io.in.rd := stateRead 
  io.out.wr := RegNext(stateWrite) 


  io.out.din := readReg 
  io.out.mask := Fill(io.out.maskWidth, 1.U)
}
