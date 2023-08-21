package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Hardware definition
class Convolution(iwl: Int, fwl:  Int) extends Component {
  var rrc_taps = Vec(AFix.S(2 exp, -12 exp), 43)
  rrc_taps(0) := 0.01897049
  rrc_taps(0).raw.msb := True
  rrc_taps(1) := 0.01842133
  rrc_taps(1).raw.msb := True
  rrc_taps(2) := 0.00954316
  rrc_taps(2).raw.msb := True
  rrc_taps(3) := 0.00624239
  rrc_taps(4) := 0.02459163
  rrc_taps(5) := 0.03898283
  rrc_taps(6) := 0.04231734
  rrc_taps(7) := 0.02906669
  rrc_taps(8) := 0.00255221
  rrc_taps(8).raw.msb := True
  rrc_taps(9) := 0.04897974
  rrc_taps(9).raw.msb := True
  rrc_taps(10) := 0.10083352
  rrc_taps(10).raw.msb := True
  rrc_taps(11) := 0.1438624
  rrc_taps(11).raw.msb := True
  rrc_taps(12) := 0.16131922
  rrc_taps(12).raw.msb := True
  rrc_taps(13) := 0.13737078
  rrc_taps(13).raw.msb := True
  rrc_taps(14) := 0.0608575
  rrc_taps(14).raw.msb := True
  rrc_taps(15) := 0.07144658
  rrc_taps(16) := 0.25288767
  rrc_taps(17) := 0.46687654
  rrc_taps(18) := 0.68890519
  rrc_taps(19) := 0.89035557
  rrc_taps(20) := 1.04339768
  rrc_taps(21) := 1.12599805
  rrc_taps(22) := 1.12599805
  rrc_taps(23) := 1.04339768
  rrc_taps(24) := 0.89035557
  rrc_taps(25) := 0.68890519
  rrc_taps(26) := 0.46687654
  rrc_taps(27) := 0.25288767
  rrc_taps(28) := 0.07144658
  rrc_taps(29) := 0.0608575
  rrc_taps(29).raw.msb := True
  rrc_taps(30) := 0.13737078
  rrc_taps(30).raw.msb := True
  rrc_taps(31) := 0.16131922
  rrc_taps(31).raw.msb := True
  rrc_taps(32) := 0.1438624
  rrc_taps(32).raw.msb := True
  rrc_taps(33) := 0.10083352
  rrc_taps(33).raw.msb := True
  rrc_taps(34) := 0.04897974
  rrc_taps(34).raw.msb := True
  rrc_taps(35) := 0.00255221
  rrc_taps(35).raw.msb := True
  rrc_taps(36) := 0.02906669
  rrc_taps(37) := 0.04231734
  rrc_taps(38) := 0.03898283
  rrc_taps(39) := 0.02459163
  rrc_taps(40) := 0.00624239
  rrc_taps(41) := 0.00954316
  rrc_taps(41).raw.msb := True
  rrc_taps(42) := 0.01842133
  rrc_taps(42).raw.msb := True
  val io = new Bundle {
    var signal = slave  Flow(AFix.S(iwl exp, fwl exp))
    var result = master Flow(AFix.S(iwl exp, fwl exp))
  }
  //val mul = Vec(Reg(inputDataType), width)
  //val signal_hist = Vec(Reg(inputDataType), width)
  val mul = Vec.fill(rrc_taps.length)(Reg(AFix.S(iwl exp, fwl exp)) init (0))
  val signal_hist = Vec.fill(rrc_taps.length)(Reg(AFix.S(iwl exp, fwl exp)) init(0))
  
  val fsm = new StateMachine {
    io.result.setIdle()
    val sum = Reg(AFix.S(iwl exp, fwl exp)) init (0)
    val idle: State = new State with EntryPoint {
      whenIsActive {
        for (index <- 1 until rrc_taps.length) {
          signal_hist(index) := signal_hist(index-1)
        }
        when(io.signal.valid) {
          signal_hist(0) := io.signal.payload
          goto(multiply)
        }
      }
    }

    val multiply: State = new State {
      whenIsActive {
          for( i <- 0 until rrc_taps.length) {
            val res = (signal_hist(i) * rrc_taps(i))
            //report(Seq(res.asSInt(), " = ", signal_hist(i).asSInt(), " * ", rrc_taps(i).asSInt()))
            mul(i) := res.saturated
          }
          goto(calcResult)
       }
    }

    val calcResult: State = new State {
      whenIsActive {
        sum := mul.reduce(_ + _).saturated
        goto(pushResult)
      }
    }

    val pushResult: State = new State {
      whenIsActive {
        report(Seq("sum: ", sum.asSInt()))
        io.result.push(sum)
        goto(idle)
      }
    }
  }
}



case class FirFilter() extends Component {
  val io = new Bundle {
    var result = out SInt(15 bits)
  }
  val filter = new Convolution(2, -12)
  val counter = Reg(UInt(6 bits)) init (0)
  val pushSignal = master Flow(AFix.S(2 exp, -12 exp))
  filter.io.signal << pushSignal 
  //pushSignal >> filter.io.signal

  val fsm = new StateMachine {
    val res = Reg(SInt(15 bits))
    io.result := 0
    res := 0
    val pushInput: State = new State with EntryPoint {
      pushSignal.setIdle()
      whenIsActive {
        when (counter === 0) { 
          val tmp = AFix.S(2 exp, -12 exp)
          tmp := 1.0
          pushSignal.push(tmp)
        } otherwise { 
          val tmp = AFix.S(2 exp, -12 exp)
          tmp := 0.0
          pushSignal.push(tmp)
        }
        goto(pullResult)
      }
    }

    val pullResult: State = new State {
      whenIsActive {
        when (filter.io.result.valid) {
          res := filter.io.result.toReg().asSInt()
          io.result := res
          counter := counter + 1
          goto(pushInput)
        }
      }
    }
  }
}

object MyTopLevelVerilog extends App {
  Config.spinal.generateVerilog(FirFilter())
}

object MyTopLevelVhdl extends App {
  Config.spinal.generateVhdl(FirFilter())
}
