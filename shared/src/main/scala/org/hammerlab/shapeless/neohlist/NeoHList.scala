package org.hammerlab.shapeless.neohlist

import shapeless._

//sealed trait OHList[In <: HList] {
//  type Out <: HList
//}
//object OHList {
//  type Aux[In, O] = OHList[In] { type Out = O }
//  implicit val hnil: Aux[HNil, HNil] = new OHList[HNil] { type Out = HNil }
//  implicit def cons[H, T <: HList](implicit t: OHList[T]): OHList[Option[H] :: t.Out] =
//    new OHList[Option[H] :: t.Out] {
//      override type Out = this.type
//    }
//}

sealed trait OHList[+L <: HList]

object OHList {
  case object nil extends OHList[HNil]
  case class Cons[+H, +L <: HList](h: Option[H], t: OHList[L]) extends OHList[H :: L]

  sealed trait Empty[L <: HList] {
    def apply(): OHList[L]
  }
  object Empty {
    implicit val nil: Empty[HNil] = new Empty[HNil] { def apply() = OHList.nil }
    implicit def cons[H, T <: HList](implicit t: Empty[T]): Empty[H :: T] = new Empty[H :: T] { def apply() = Cons(None, t()) }
  }

  def empty[L <: HList](implicit e: Empty[L]): OHList[L] = e()
}

sealed trait NeoHList[+H, +L <: HList] /*extends OHList[H :: L]*/ {
  //type H_? <: Option[Head]
  //def head: H_?
  //def head: Option[Head]

//  type Tail
//  def tail: Tail
//  type H
//  type T <: HList
//  type Tail <: OHList[T]
  //type NonEmptyTail <: NeoHList
}
object NeoHList {
//  case class Base[H](h: H) extends NeoHList[H, HNil] {
//    def head = Some(h)
//  }

//  def apply[H](h: H): Cons[H, HNil] = new Cons(h, OHList.nil)
//  def apply[H, L <: HList](h: H, t: OHList[L]): Cons[H, L] = Cons(h, t)
//  def apply[H1, H2, L <: HList](t: NeoHList[H2, L]): Extend[H1, H2, L] = Extend(t)

  case class Cons[+H, +L <: HList] (h: H, t: OHList[L]) extends NeoHList[H, L]
//  object Cons {
//    def make[H](h: H): Cons[H, HNil] = new Cons(h, OHList.nil)
    //def unapply[H, L <: HList](c: Cons[H, L]): Option[(H, OHList[L])] = Some((c.h, c.t))
//  }
  case class Extend[+H1, +H2, +L <: HList](t: NeoHList[H2, L]) extends NeoHList[H1, H2 :: L]
}
