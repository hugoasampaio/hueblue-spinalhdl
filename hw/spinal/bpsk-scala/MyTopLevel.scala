package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Hardware definition
case class Convolution(width: Int) extends Component {
  val io = new Bundle {
    val signal    = slave Flow(SInt(16 bits))
    val fir_coeff = in  Vec(SInt(16 bits), width)
    val result =    master Flow(SInt(32 bits))
  }
  val mul = Vec(Reg(SInt(32 bits)), width)
  val signal_hist = Vec(Reg(SInt(16 bits)), width)
  signal_hist.map(_ := 0)

  val fsm = new StateMachine {
    io.result.setIdle()

    val idle: State = new State with EntryPoint {
      whenIsActive {
        when(io.signal.valid) {
          //shift
          for (index <- width-1 to 1)
            signal_hist(index) := signal_hist(index-1)
          signal_hist(0) := io.signal.payload

          //multiply
          for( i <- 0 to width-1)
            mul(i) := signal_hist(i) * io.fir_coeff(i)
                goto(pushResult)
        }
      }
    }

    val pushResult: State = new State {
      whenIsActive {
        //sum
        io.result.push(mul.reduceLeft(_ + _))
        goto(idle)
      }
    }
  }   
}

object MyTopLevelVerilog extends App {
  Config.spinal.generateVerilog(Convolution(43))
}

object MyTopLevelVhdl extends App {
  Config.spinal.generateVhdl(Convolution(43))
}
