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

case class FirFilter() extends Component {
  var coeff = Vec(SInt(16 bits), 43)
    coeff(0) := 186
    coeff(1) := 233
    coeff(2) := 248
    coeff(3) := 217
    coeff(4) := 134
    coeff(5) := 0
    coeff(6) := -173
    coeff(7) := -364
    coeff(8) := -540
    coeff(9) := -665
    coeff(10) := -700
    coeff(11) := -613
    coeff(12) := -382
    coeff(13) := 0
    coeff(14) := 521
    coeff(15) := 1151
    coeff(16) := 1842
    coeff(17) := 2533
    coeff(18) := 3159
    coeff(19) := 3660
    coeff(20) := 3983
    coeff(21) := 4095
    coeff(22) := 3983
    coeff(23) := 3660
    coeff(24) := 3159
    coeff(25) := 2533
    coeff(26) := 1842
    coeff(27) := 1151
    coeff(28) := 521
    coeff(29) := 0
    coeff(30) := -382
    coeff(31) := -613
    coeff(32) := -700
    coeff(33) := -665
    coeff(34) := -540
    coeff(35) := -364
    coeff(36) := -173
    coeff(37) := 0
    coeff(38) := 134
    coeff(39) := 217
    coeff(40) := 248
    coeff(41) := 233
    coeff(42) := 186
  
  var filter = Convolution(43)
  filter.io.fir_coeff := coeff
  val counter = Reg(UInt(7 bits)) init (0)
  val fsm = new StateMachine {

    val pushInput: State = new State with EntryPoint {
      whenIsActive {
        if (counter == 0)
          filter.io.signal.push(1)
        else 
          filter.io.signal.payload(0)
        goto(pullResult)
      }
    }

    val pullResult: State = new State {
      whenIsActive {
        val ans = filter.io.result.toReg()
        println(ans)
        counter := counter + 1
        goto(pushInput)
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
