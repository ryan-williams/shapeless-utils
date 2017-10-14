package org.hammerlab.shapeless

import org.hammerlab.shapeless.Find.Ops
import shapeless._
import shapeless.ops.record.Selector

/**
 * Type-class for recursively selecting a field [[K]] in an object [[C]]
 */
trait Find[C, K] {
  type V
  def apply(c: C): V
}

trait LowPriFind {
  type Aux[C, K, V0] = Find[C, K] { type V = V0 }

  def make[C, K, V0](fn: C ⇒ V0): Aux[C, K, V0] =
    new Find[C, K] {
      type V = V0
      override def apply(c: C): V0 = fn(c)
    }

  /**
   * Extend a [[Find]] when an arbitrary element is prepended
   *
   * This needs to be lower-priority than [[Find.fromSelector]], because we prefer to recurse
   */
  implicit def continue[H, L <: HList, K](implicit findTail: Find[L, K]): Aux[H :: L, K, findTail.V] =
    make(c ⇒ findTail(c.tail))
}

object Find
  extends LowPriFind
    with HasFindOps {

  def apply[C, K](implicit find: Find[C, K]): Aux[C, K, find.V] = find

  /** Bridge a case-class to its [[LabelledGeneric]] */
  implicit def fromCC[CC, L <: HList, K](implicit gen: LabelledGeneric.Aux[CC, L], find: Find[L, K]): Aux[CC, K, find.V] =
    make(cc ⇒ find(gen.to(cc)))

  /** Recurse from a case-class to any of its fields, via its [[Generic]] */
  implicit def fromCCRec[CC, L <: HList, K](implicit gen: Generic.Aux[CC, L], find: Lazy[Find[L, K]]): Aux[CC, K, find.value.V] =
    make(cc ⇒ find.value(gen.to(cc)))

  /** Convert a [[Selector]] to a [[Find]] */
  implicit def fromSelector[C <: HList, K](implicit sl: Selector[C, K]): Aux[C, K, sl.Out] =
    make(sl(_))

  /** Construct a [[Find]] by prepending an existing [[Find]] onto any [[HList]] */
  implicit def directCons[H, K, L <: HList](implicit findHead: Lazy[Find[H, K]]): Aux[H :: L, K, findHead.value.V] =
    make(c ⇒ findHead.value(c.head))

  class Ops[T](val t: T) extends AnyVal {
    def find(w: Witness)(implicit f: Find[T, w.T]): f.V = f(t)
  }
}

trait HasFindOps {
  implicit def makeFindOps[T](t: T): Ops[T] = new Ops(t)
}
