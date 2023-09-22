package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import LimitedFix._
import Constants._

// Hardware definition

class Convolution extends Component {
  val rrc_taps = new RRC_FILTER().rrc_taps
  val io = new Bundle {
    val signal = slave  Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))
    val result = master Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))
  }

  val mul =     Vec.fill(rrc_taps.length)(Reg(LimitedFix(AFix.S(Constants.IWL exp, Constants.FWL exp), True)))
  val sigHist = Vec.fill(rrc_taps.length)(Reg(AFix.S(Constants.IWL exp, Constants.FWL exp)) init(0))
  val sum = Reg(LimitedFix(AFix.S(Constants.IWL exp, Constants.FWL exp), True)) init(0)
  
  val fsm = new StateMachine {
    io.result.setIdle()
    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.signal.valid) {
          sigHist(0) := (io.signal.payload)
          report("beforeSift")
          for (index <-  1 until rrc_taps.length) {
            sigHist(index) := sigHist(index - 1)
          }
          goto(multiply)
        }
      }
    }

    val multiply: State = new State {
      whenIsActive {
        report("beforeMul")
        for( i <- 0 until rrc_taps.length) {
          mul(i) := LimitedFix((sigHist(i) * (rrc_taps(i).getFxp())).saturated, True)
          //report(Seq("tmp: ", mul(i).asSInt, " len: ", mul(i).bitWidth.toString()))
        }
        goto(reduceStep)
      }
    }

    val reduceStep: State = new State {
      whenIsActive {
        report("beforeReduce")
        sum := mul.reduce(_ + _)
        goto(returnValue)
       }
    }

    val returnValue: State = new State {
      whenIsActive {
        report(Seq("sum: ", sum.getFxp().asSInt))
        io.result.push(sum.getFxp())
        goto(idle)
      }
    }
  }
}



case class FirFilter() extends Component {
  val io = new Bundle {
    val result = out SInt(15 bits)
  }
  val filter = new Convolution()
  val counter = Reg(UInt(6 bits)) init (0)
  val pushSignal = master Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))
  filter.io.signal << pushSignal 

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
