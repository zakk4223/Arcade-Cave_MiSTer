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

package arcadia.mem.cache

import arcadia.mem._
import arcadia.mem.request.ReadRequest
import chisel3._
import chisel3.util._

/**
 * A 2-way set-associative read-only cache memory.
 *
 * A cache can be used to speed up access to a high latency memory device (e.g. SDRAM), by keeping a
 * copy of frequently accessed data.
 *
 * Cache entries are stored in BRAM, which means that reading a memory address that is already
 * stored in the cache (cache hit) only takes one clock cycle.
 *
 * Reading a memory address that isn't stored in the cache (cache miss) is slower, as the
 * data must first be fetched from the high latency memory device (line fill). Although, subsequent
 * reads/writes to the same address will be much faster.
 *
 * @param config The cache configuration.
 */
class ReadCache(config: Config) extends Module {
  // Sanity check
  assert(config.inDataWidth >= 8, "Input data width must be at least 8")
  assert(config.outDataWidth >= 8, "Output data width must be at least 8")

  val io = IO(new Bundle {
    /** Enable the cache */
    val enable = Input(Bool())
    /** Input port */
    val in = Flipped(AsyncReadMemIO(config.inAddrWidth, config.inDataWidth))
    /** Output port */
    val out = BurstReadMemIO(config.outAddrWidth, config.outDataWidth)
    /** Debug port */
    val debug = Output(new Bundle {
      val idle = Bool()
      val check = Bool()
      val fill = Bool()
      val fillWait = Bool()
      val write = Bool()
      val lru = Bits(config.depth.W)
    })
  })

  // States
  object State {
    val init :: idle :: check :: fill :: fillWait :: write :: Nil = Enum(6)
  }

  // State register
  val stateReg = RegInit(State.init)

  val isInit = RegInit(true.B)
  val isIdle = RegInit(false.B)
  val isCheck = RegInit(false.B) 
  val isFill = RegInit(false.B) 
  val isFillwait = RegInit(false.B)
  val isWrite = RegInit(false.B) 
  // Start request flag
  val start = Wire(Bool())

  // Convert input address to a word offset
  val offsetReg = {
    val addr = io.in.addr >> log2Ceil(config.inBytes)
    val offset = addr(log2Up(config.inWords) - 1, 0)
    RegEnable(offset, start)
  }

  // Cache request
  val request = ReadRequest(io.in.rd, Address(config, io.in.addr))
  val requestReg = RegEnable(request, start)

  // Data registers
  val doutReg = Reg(Bits(config.inDataWidth.W))
  val validReg = RegNext(false.B)

  // Least recently used register
  val lruReg = Reg(Bits(config.depth.W))

  // Cache way register
  val nextWay = Wire(Bool())
  val wayReg = RegNext(nextWay)

  // Cache entry memory
  val cacheEntryMemA = SyncReadMem(config.depth, new Entry(config))
  val cacheEntryMemB = SyncReadMem(config.depth, new Entry(config))

  // Cache entry for the current index
  val cacheEntryA = cacheEntryMemA.read(request.addr.index)
  val cacheEntryB = cacheEntryMemB.read(request.addr.index)

  // Latch cache entry for the current way during the check state. This prevents the cache entry
  // register from changing during a request.
  val cacheEntryReg = RegEnable(Mux(nextWay, cacheEntryB, cacheEntryA), isCheck)

  // Write the cache entry
  val nextCacheEntry = Mux(isWrite, cacheEntryReg, Entry.zero(config))

  when(isInit || (isWrite && !wayReg)) {
    cacheEntryMemA.write(requestReg.addr.index, nextCacheEntry)
  }

  when(isInit || (isWrite && wayReg)) {
    cacheEntryMemB.write(requestReg.addr.index, nextCacheEntry)
  }

  // Assert the burst counter enable signal as words are bursted from memory
  val burstCounterEnable = isFillwait && io.out.valid

  // Counters
  val (initCounter, initCounterWrap) = Counter(isInit, config.depth)
  val (burstCounter, burstCounterWrap) = Counter(burstCounterEnable, config.lineWidth)

  // Control signals
  start := io.enable && request.rd && isIdle 
  val wait_n = io.enable && isIdle 
  val hitA = cacheEntryA.isHit(requestReg.addr)
  val hitB = cacheEntryB.isHit(requestReg.addr)
  val hit = hitA || hitB
  val miss = !hit

  // For a wrapping burst, the word done signal is asserted as soon as enough output words have
  // been bursted to fill an input word. Otherwise, for a non-wrapping burst we need to wait until
  // the burst is complete.
  val wordDone = if (config.wrapping) {
    burstCounter === (config.inOutDataWidthRatio - 1).U
  } else {
    burstCounterWrap
  }

  // Calculate the output byte address
  val outAddr = {
    val addr = if (!config.wrapping) {
      Address(config, requestReg.addr.tag, requestReg.addr.index, 0.U)
    } else {
      requestReg.addr
    }
    (addr.asUInt << log2Ceil(config.outBytes)).asUInt
  }

  // Latch current cache way when a request is started
  nextWay := Mux(start, lruReg(request.addr.index), wayReg)

  // Fill the cache line as words are bursted from memory
  when(isFillwait && io.out.valid) {
    val n = if (config.wrapping) requestReg.addr.offset + burstCounter else burstCounter
    val entry = cacheEntryReg.fill(requestReg.addr.tag, n, io.out.dout)
    cacheEntryReg := entry
    doutReg := entry.inWord(offsetReg)
    validReg := requestReg.rd && wordDone
  }

  def onHit() = {
    isCheck := false.B
    isIdle := true.B
    doutReg := Mux(hitA, cacheEntryA.inWord(offsetReg), cacheEntryB.inWord(offsetReg))
    validReg := true.B
    lruReg := lruReg.bitSet(requestReg.addr.index, hitA)
    nextWay := !hitA
  }

  def onMiss() = {
    isCheck := false.B
    isFill := true.B
    lruReg := lruReg.bitSet(requestReg.addr.index, !wayReg)
  }

  // FSM

  when(isInit && initCounterWrap) {
    isInit := false.B
    isIdle := true.B
  }.elsewhen(isIdle && start) {
    isIdle := false.B
    isCheck := true.B
  }.elsewhen(isCheck) {
    when(hit) {
      onHit()
    }.elsewhen(miss) {
      onMiss()
    }
  }.elsewhen(isFill && io.out.wait_n) {
    isFill := false.B
    isFillwait := true.B
  }.elsewhen(isFillwait && burstCounterWrap) {
    isFillwait := false.B
    isWrite := true.B
  }.elsewhen(isWrite) {
    isWrite := false.B
    isIdle := true.B
  }
   
  /*
  switch(stateReg) {
    // Initialize cache
    is(State.init) {
      when(initCounterWrap) { stateReg := State.idle }
    }

    // Wait for a request
    is(State.idle) {
      when(start) { stateReg := State.check }
    }

    // Check cache entry
    is(State.check) {
      when(hit) { onHit() }.elsewhen(miss) { onMiss() }
    }

    // Fill a cache line
    is(State.fill) {
      when(io.out.wait_n) { stateReg := State.fillWait }
    }

    // Wait for a line file
    is(State.fillWait) {
      when(burstCounterWrap) { stateReg := State.write }
    }

    // Write cache entry
    is(State.write) { stateReg := State.idle }
  }
  */

  // Outputs
  io.in.wait_n := wait_n
  io.in.valid := validReg
  io.in.dout := doutReg
  io.out.rd := isFill 
  io.out.burstLength := config.lineWidth.U
  io.out.addr := outAddr
  io.debug.idle := isIdle
  io.debug.check := isCheck 
  io.debug.fill := isFill 
  io.debug.fillWait := isFillwait 
  io.debug.write := isWrite 
  io.debug.lru := lruReg

  // Debug
  if (sys.env.get("DEBUG").contains("1")) {
    printf(p"CacheMem(state: $stateReg, way: $wayReg, lru: 0x${Hexadecimal(lruReg)}, hitA: $hitA, hitB: $hitB, tag: 0x${Hexadecimal(requestReg.addr.tag)}, index: ${requestReg.addr.index}, offset: $offsetReg, wordsA: 0x${Hexadecimal(cacheEntryA.line.inWords.asUInt)} (0x${Hexadecimal(cacheEntryA.tag)}), wordsB: 0x${Hexadecimal(cacheEntryB.line.inWords.asUInt)} (0x${Hexadecimal(cacheEntryB.tag)}))\n")
  }
}
