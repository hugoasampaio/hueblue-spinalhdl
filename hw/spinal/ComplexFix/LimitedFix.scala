package LimitedFix

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import scala.collection.mutable.ArrayBuffer

object LimitedFix {
    def apply(fxp: AFix): LimitedFix = {
        new LimitedFix(fxp)
    }
}

class LimitedFix(fxp: AFix) extends MultiData {

    var zeroesBitMask = Reg(Bits(fxp.bitWidth bits))
    zeroesBitMask.setAll()
    zeroesBitMask.lsb := False

    def assignFromImpl(that: AnyRef,target: AnyRef,kind: AnyRef)
        (implicit loc: spinal.idslplugin.Location): Unit = {
        }

    def elements: scala.collection.mutable.ArrayBuffer[(String, spinal.core.Data)] = {
        ArrayBuffer("" -> fxp.raw)
    }

    def init(num: BigDecimal): LimitedFix ={
        fxp := num
        this
    }
    
    def getFxp(): AFix = fxp

    def setFxp(tmp: AFix): Unit = fxp := tmp

    def applyBaseMask(): Unit = {
        var tmp = fxp.raw & zeroesBitMask
        fxp.raw := tmp
        report(" limited ")
    }

    def +(that: LimitedFix): LimitedFix = {
        val tmp = AFix.S(2 exp, -12 exp)
        tmp := fxp +| that.getFxp()
        LimitedFix(tmp.saturated)
    }

    def *(that: LimitedFix): LimitedFix = {
        this * that.getFxp()
    }

    def *(that: AFix): LimitedFix = {
        val tmp = AFix.S(2 exp, -12 exp)
        tmp := fxp * that
        LimitedFix(tmp.saturated)
    }

    def :=(that: LimitedFix): LimitedFix = {
        val tmp = AFix.S(2 exp, -12 exp)
        //report(Seq("bits: ", tmp.asSInt))
        tmp := that.getFxp()
        this.fxp.raw := tmp.raw & zeroesBitMask
        this
    }
}
