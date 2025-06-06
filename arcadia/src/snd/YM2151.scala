package arcadia.snd

import arcadia.clk.ClockDivider
import arcadia.mem._
import chisel3._
import chisel3.util._

/**
 * The YM2203 is a FM sound synthesizer.
 *
 * @param clockFreq  The system clock frequency (Hz).
 * @param sampleFreq The sample clock frequency (Hz).
 * @note This module wraps jotego's JT12 implementation.
 * @see https://github.com/jotego/jt12
 */
class YM2151(clockFreq: Double, sampleFreq: Double) extends Module {
  val io = IO(new Bundle {
    /** CPU port */
    val cpu = Flipped(MemIO(1, 8))
    /** IRQ */
    val irq = Output(Bool())
    /** Audio output port */
    val audio = ValidIO(new Bundle {
      val left = SInt(16.W)
      val right = SInt(16.W)
    })
  })

  class JT51_ extends BlackBox {
    val io = IO(new Bundle {
      val rst = Input(Bool())
      val clk = Input(Bool())
      val cen = Input(Bool())
      val cen_p1 = Input(Bool())
      val din = Input(Bits(8.W))
      val a0 = Input(Bool())
      val cs_n = Input(Bool())
      val wr_n = Input(Bool())
      val dout = Output(Bits(8.W))
      val ct1 = Output(Bool())
      val ct2 = Output(Bool())
      val irq_n = Output(Bool())
      val left = Output(SInt(16.W))
      val right = Output(SInt(16.W))
      val xleft = Output(SInt(16.W))
      val xright = Output(SInt(16.W))
      val sample = Output(Bool())
    })

    override def desiredName = "jt51"
  }

  val m = Module(new JT51_)
  m.io.rst := reset.asBool
  m.io.clk := clock.asBool
  m.io.cen := ClockDivider(clockFreq / sampleFreq)
  m.io.cen_p1 := ClockDivider(clockFreq / (sampleFreq/2))
  m.io.cs_n := false.B
  m.io.wr_n := !io.cpu.wr
  m.io.a0 := io.cpu.addr(0)
  m.io.din := io.cpu.din
  io.cpu.dout := m.io.dout
  io.irq := !m.io.irq_n
  io.audio.valid := m.io.sample
  io.audio.bits.left := m.io.xleft
  io.audio.bits.right := m.io.xright
}
