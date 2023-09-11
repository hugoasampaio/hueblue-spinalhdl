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
    }

    def +(that: LimitedFix): LimitedFix = {
        var tmp = fxp +| that.getFxp()
        LimitedFix(tmp.truncated)
    }

    def *(that: LimitedFix): LimitedFix = {
        var tmp = fxp * that.getFxp()
        LimitedFix(tmp.saturated)
    }

    def *(that: AFix): LimitedFix = {
        var tmp = fxp * that
        LimitedFix(tmp.saturated)
    }

    def :=(that: LimitedFix): LimitedFix = {
        this.fxp := that.getFxp().saturated
        when(fxp === that.getFxp()) { 
            applyBaseMask()
        }
        this
    }
}
