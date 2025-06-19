
package cave.gfx

import arcadia.mem._
import arcadia.mem.arbiter.BurstMemArbiter
import arcadia.util.Counter
import arcadia.Util
import cave._
import chisel3.util._
import chisel3._

class SpriteDescrambler(addrWidth: Int, dataWidth: Int) extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())
    val gameConfig = Input(GameConfig())

    /** memory port */
    val in = BurstReadMemIO(addrWidth, dataWidth)
    val out = BurstWriteMemIO(addrWidth, dataWidth)
  })


  val readOffset = RegInit(UInt(16.W), 0.U)
  val writeOffset = RegInit(UInt(32.W), 0.U)
  val writeCount = RegInit(UInt(16.W), 0.U) 
  val regIdx = RegInit(UInt(4.W), 0.U)

  val readRegs = Reg(Vec(4, UInt(16.W)))


  val sIdle :: sRead :: sReadWait :: sReadValid :: sWrite :: sWriteDone :: sDone :: Nil = Enum(7)
  val state = RegInit(sIdle)
  val write = RegInit(false.B)
  val read = RegInit(false.B)
  val done = RegInit(false.B)

  io.done := false.B


  val readAddr = {
    val descramble_0 =  writeOffset + readOffset 
    val xorRes_1 = descramble_0 ^ 0x950c4.U(32.W)
    val descramble_1 = Cat(Seq(23,22,21,20,15,10,12,6,11,1,13,3,16,17,2,5,14,7,18,8,4,19,9,0)
      .map(Util.decode(xorRes_1, 24, 1).apply).toSeq)

    val xorRes_2 = descramble_0 ^ 0xdf88.U(32.W)
    val descramble_2 = Cat(Seq(23,22,21,20,19,9,7,3,15,4,17,14,18,2,16,5,11,8,6,13,1,10,12,0)
      .map(Util.decode(xorRes_1, 24, 1).apply).toSeq)

    MuxCase(descramble_0, Seq(
      (io.gameConfig.sprite.descrambleStyle === 0.U) -> descramble_0,
      (io.gameConfig.sprite.descrambleStyle === 1.U) -> descramble_1,
      (io.gameConfig.sprite.descrambleStyle === 2.U) -> descramble_2
    ))
  }

  val writeAddr = {
     writeOffset
  }

  val resultOffset = readAddr(2,0)


  state := state

  switch(state) {
    is(sIdle) {
      when(io.start) {
        state := sRead
      }
    }

    is (sRead) {
        when (io.in.valid) {
          state := sReadValid
        }.elsewhen(io.in.wait_n) {
          state := sReadWait
        }
    }

    is (sReadWait) {
      when (io.in.valid && io.in.burstDone) {
        state := sReadValid
      }
    }

    is (sReadValid) {
      readRegs(regIdx) := (io.in.dout >> (resultOffset << 3))(15,0) 
      state := sRead
      readOffset := readOffset + 2.U
      regIdx := regIdx + 1.U
      when (regIdx === 3.U) {
        state := sWrite
      }
    }

    is (sWrite) {
        when(io.out.wait_n) {
          state := sWriteDone
        }
    }

    is (sWriteDone) {
        writeOffset := writeOffset + 8.U
        when (writeOffset + 8.U < io.gameConfig.sprite.romSize) {
          state := sIdle
          readOffset := 0.U
          regIdx := 0.U
        }.otherwise {
          state := sDone
        }

    }
    is (sDone) {
      done := true.B
    }
  }

  io.in.addr := readAddr & (~7.U(64.W))
  io.out.addr := writeAddr
  io.in.burstLength := 1.U
  io.out.burstLength := 1.U
  io.done := done
  io.in.rd := state === sRead
  io.out.wr := state === sWrite 


  io.out.din := Cat(readRegs(3), readRegs(2), readRegs(1), readRegs(0))
  io.out.mask := Fill(io.out.maskWidth, 1.U)
}
