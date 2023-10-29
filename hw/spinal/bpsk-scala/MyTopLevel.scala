package bpskscala

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import LimitedFix._
import Constants._
// Hardware definition

case class Convolution() extends Component {
  //report("bundle")
  val io = new Bundle {
    val signal = slave  Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))
    val result = master Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))

  }
  val coeffs = RRC_FILTER().rrc_taps

  //report("signals")
  val mul =     Vec.fill(coeffs.length)(Reg(LimitedFix(AFix.S(Constants.IWL exp, Constants.FWL exp), 2)) init(0))
  val sigHist = Vec.fill(coeffs.length)(Reg(AFix.S(Constants.IWL exp, Constants.FWL exp)) init(0))
  val sum = Reg(LimitedFix(AFix.S(Constants.IWL exp, Constants.FWL exp), 3)) init(0)

  val fsm = new StateMachine {
    val idle: State = new State with EntryPoint {
      io.result.setIdle()
      whenIsActive {
        when(io.signal.valid) {
          sigHist(0) := (io.signal.payload)
          //report("beforeSift")
          for (index <- 1 until coeffs.length) {
            sigHist(index) := sigHist(index - 1)
          }
          goto(multiply)
        }
      }
    }

    val multiply: State = new State {
      whenIsActive {
        //report("beforeMul")
        for( i <- 0 until coeffs.length) {
          mul(i) := LimitedFix((sigHist(i) * (coeffs(i).getFxp())).saturated, 0)
          //report(Seq("tmp: ", mul(i).asSInt, " len: ", mul(i).bitWidth.toString()))
        }
        goto(reduceStep)
      }
    }

    val reduceStep: State = new State {
      whenIsActive {
        //report("beforeReduce")
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

  val filter =  Convolution()
  val masks = Vec.fill(10) (Reg(Bits(15 bits)) init(B"15'h7fff"))
  val counter = Reg(UInt(6 bits)) init (0)
  val pushSignal = master Flow(AFix.S(Constants.IWL exp, Constants.FWL exp))

  filter.io.signal << pushSignal

  val fsm = new StateMachine {
    val res = Reg(SInt(15 bits)) init(0)
    io.result := 0
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
      val fullLoop = Reg(UInt(4 bits)) init (0)
      whenIsActive {
        when (filter.io.result.valid) {
          res := filter.io.result.toReg().asSInt()
          io.result := res
          when (counter > RRC_FILTER().rrc_taps.length) {
            counter := 0
            fullLoop := fullLoop + 1

            val shiftedMask = B"15'h7fff" |<< fullLoop
            masks(1) := shiftedMask
            report(Seq("shiftedMask: ", shiftedMask))

          } otherwise {
            counter := counter + 1
          }
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


object FindAllAddersManualy {
  import spinal.core.internals._
  import spinal.core._

  class PrintBaseTypes(message : String) extends Phase {

    override def impl(pc: PhaseContext) = {
      println(message)
      pc.walkExpression {
        //case op: Operator.BitVector.Add => println(s"op: ${op.left} + ${op.right}")
        case bs: BaseType => recBaseType(bs)
        case as: AssignmentExpression => recAssignExp(as)
        case op: ConstantOperator => println(s"const op: ${op.opName}")
        //case ss:
        case _ =>
      }

      def recAssignExp(ae: AssignmentExpression): Unit = {
        ae.walkDrivingExpressions{
          case a: LimitedFix => println(s"assign exp: ${a.name}")
          case _ =>
        }
      }

      def recBaseType(b: BaseType): Unit = {

        if (b.isTypeNode == false) {
          println(s"name: ${b.name} - class: ${b.getDisplayName()} - opname: ${b.opName} - name: ${b.name}")
        }

        //println(s"tag: ${b.getTag(SpinalTagReady.getClass).toString}")
        /*
        val tags = b.getTag(LimitedFix.getClass)
        tags match {
          case as : SpinalTag => println(s"attr: ${as.toString()}")
          case _ => 
        }
        */
      }
    }

    override def hasNetlistImpact = false

    override def toString = s"${super.toString} - $message"
  }

  def main(args: Array[String]): Unit = {
    val config = SpinalConfig()

    //Add a early phase
    config.addTransformationPhase(new PrintBaseTypes("Early"))

    //Add a late phase
    config.phasesInserters += {phases =>
      phases.insert(phases.indexWhere(_.isInstanceOf[PhaseVerilog]), new PrintBaseTypes("Late"))
    }
    config.generateVerilog(new FirFilter())
  }

}