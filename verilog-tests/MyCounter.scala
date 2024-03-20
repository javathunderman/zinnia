//> using scala "2.13.12"
//> using dep "org.chipsalliance::chisel::6.2.0"
//> using plugin "org.chipsalliance:::chisel-plugin::6.2.0"
//> using options "-unchecked", "-deprecation", "-language:reflectiveCalls", "-feature", "-Xcheckinit", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ywarn-unused", "-Ymacro-annotations"

import chisel3._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage
import chisel3.util._


class MyCounter(maxVal: Int) extends Module {
    val io = IO(new Bundle {
        val en  = Input(Bool())
        val out = Output(UInt())
    })
    val count = Reg(UInt(log2Ceil(maxVal+1).W))
    val nextVal = Mux(count < maxVal.U, count + 1.U, 0.U)
    val applyEn = Mux(io.en, nextVal, count)
    count := Mux(reset.asBool, 0.U, applyEn)
    io.out := count
}

object Main extends App {
  println(
    ChiselStage.emitSystemVerilog(
      gen = new MyCounter(15),
      firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
    )
  )
}