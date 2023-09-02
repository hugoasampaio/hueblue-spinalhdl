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

    var baseMask = Bits(fxp.bitWidth bits).setAll()

    def assignFromImpl(that: AnyRef,target: AnyRef,kind: AnyRef)
        (implicit loc: spinal.idslplugin.Location): Unit = {}

    def elements: scala.collection.mutable.ArrayBuffer[(String, spinal.core.Data)] = {
        ArrayBuffer("" -> fxp.raw)
    }


    def init(num: BigDecimal): LimitedFix ={
        fxp := num
        this
    }
    
    def getFxp(): AFix = fxp

    def clearFwlBits(numBits: Int) {
        for (index <- 0 to numBits ) {
            baseMask(0) := False
        }
        fxp.raw := fxp.raw & baseMask 
    }

    def +(that: LimitedFix): LimitedFix = {
        fxp := fxp +| that.getFxp()
        fxp.raw := fxp.raw & baseMask
        this
    }

    def *(that: LimitedFix): LimitedFix = {
        var tmp = fxp * that.getFxp()
        fxp := tmp.saturated
        fxp.raw := fxp.raw & baseMask
        this
    }
}
