package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Hardware definition
case class Convolution[I <: AFix, O <: AFix](inputDataType: I, outputDataType: O, width: Int) extends Component {
  val io = new Bundle {
    var signal    = slave Flow(inputDataType)
    var fir_coeff = in  Vec(inputDataType, width)
    var result =    master Flow(outputDataType)
  }
  val mul = Vec(Reg(outputDataType), width)
  val signal_hist = Vec(Reg(inputDataType), width)
  signal_hist.map(_ := 0.0)

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
            mul(i) := (signal_hist(i) * io.fir_coeff(i))
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
  val io = new Bundle {
    var result = out SInt(32 bits)
  }
  var coeff = Vec(AFix.S(4 exp, -12 exp), 43)
    coeff(0).raw := 186
    coeff(1).raw := 233
    coeff(2).raw := 248
    coeff(3).raw := 217
    coeff(4).raw := 134
    coeff(5).raw := 0
    coeff(6).raw := -173
    coeff(7).raw := -364
    coeff(8).raw := -540
    coeff(9).raw := -665
    coeff(10).raw := -700
    coeff(11).raw := -613
    coeff(12).raw := -382
    coeff(13).raw := 0
    coeff(14).raw := 521
    coeff(15).raw := 1151
    coeff(16).raw := 1842
    coeff(17).raw := 2533
    coeff(18).raw := 3159
    coeff(19).raw := 3660
    coeff(20).raw := 3983
    coeff(21).raw := 4095
    coeff(22).raw := 3983
    coeff(23).raw := 3660
    coeff(24).raw := 3159
    coeff(25).raw := 2533
    coeff(26).raw := 1842
    coeff(27).raw := 1151
    coeff(28).raw := 521
    coeff(29).raw := 0
    coeff(30).raw := -382
    coeff(31).raw := -613
    coeff(32).raw := -700
    coeff(33).raw := -665
    coeff(34).raw := -540
    coeff(35).raw := -364
    coeff(36).raw := -173
    coeff(37).raw := 0
    coeff(38).raw := 134
    coeff(39).raw := 217
    coeff(40).raw := 248
    coeff(41).raw := 233
    coeff(42).raw := 186
  
  var filter = Convolution(AFix.S(4 exp, -12 exp), AFix.S(4 exp, -12 exp),  43)
  filter.io.fir_coeff := coeff
  val counter = Reg(UInt(6 bits)) init (0)
  val pushSignal = master Flow(AFix.S(4 exp, -12 exp))
  filter.io.signal << pushSignal

  val fsm = new StateMachine {
    val res = Reg(SInt(32 bits))
    io.result := 0

    val pushInput: State = new State with EntryPoint {
      pushSignal.setIdle()
      whenIsActive {
        if (counter == 0) {
          val tmp = AFix.S(4 exp, -12 exp)
          tmp := 1.0
          pushSignal.push(tmp)    
        } else { 
          val tmp = AFix.S(4 exp, -12 exp)
          tmp := 0.0
          pushSignal.push(tmp)
        }
        goto(pullResult)
      }
    }

    val pullResult: State = new State {
      res := 0
      counter := 0
      whenIsActive {
        res := filter.io.result.toReg().asSInt()
        io.result := res
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
