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

package arcadia.clk

import chisel3._
import chisel3.util._

/**
 * The 5205 is a ADPCM sound chip.
 *
 * @param clockFreq  The system clock frequency (Hz).
 * @param sampleFreq The sample clock frequency (Hz).
 * @note This module wraps jotego's JT5205 implementation.
 * @see https://github.com/jotego/jt5205
 */
class JTFracCen() extends Module {
  val io = IO(new Bundle {
    val n = Input(Bits(10.W))
    val m = Input(Bits(10.W))
    val cen = Output(Bits(2.W))
    val cenb = Output(Bits(2.W))
  })

  class JTFracCen_ extends BlackBox {
      val io = IO(new Bundle {
      val clk = Input(Bool())
      val cen_in = Input(Bool())
      val n = Input(Bits(10.W))
      val m = Input(Bits(10.W))
      val cen = Output(Bits(2.W))
      val cenb = Output(Bits(2.W))
    })

    override def desiredName = "jtframe_frac_cen"
  }

  val m = Module(new JTFracCen_)
  m.io.clk := clock.asBool
  m.io.cen_in := true.B
  m.io.n := io.n
  m.io.m := io.m
  io.cen := m.io.cen
  io.cenb := m.io.cenb
}
