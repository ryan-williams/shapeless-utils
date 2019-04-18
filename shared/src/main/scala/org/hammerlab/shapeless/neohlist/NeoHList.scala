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
  sealed trait Cons[+H, +L <: HList] extends OHList[H :: L] {
    def h_? : Option[H]
    def t: OHList[L]
  }
  object Cons {
    def apply[H, L <: HList](h: Option[H], t: OHList[L]): Cons[H, L] = Base(h, t)
    def unapply[H, L <: HList](cons: Cons[H, L]): Option[(Option[H], OHList[L])] = Some((cons.h_?, cons.t))
    private case class Base[+H, +L <: HList](h_? : Option[H], t: OHList[L]) extends Cons[H, L]
  }

  sealed trait Empty[L <: HList] {
    def apply(): OHList[L]
  }
  object Empty {
    implicit val nil: Empty[HNil] = new Empty[HNil] { def apply() = OHList.nil }
    implicit def cons[H, T <: HList](implicit t: Empty[T]): Empty[H :: T] = new Empty[H :: T] { def apply() = Cons(None, t()) }
  }

  def empty[L <: HList](implicit e: Empty[L]): OHList[L] = e()

  implicit val hnil: Generic.Aux[OHList[HNil], HNil] = new Generic[OHList[HNil]] {
    type Repr = HNil
    def to(t: OHList[HNil]): HNil = HNil
    def from(r: HNil): OHList[HNil] = nil
  }
  implicit def cons[H, L <: HList, OL <: HList](implicit g: Lazy[Generic.Aux[OHList[L], OL]]): Generic.Aux[OHList[H :: L], Option[H] :: OL] =
    new Generic[OHList[H :: L]] {
      type Repr = Option[H] :: OL
      def to(t: OHList[H :: L]): Repr = t match {
        case Cons(h, t) ⇒ h :: g.value.to(t)
      }
      def from(r: Repr): OHList[H :: L] = r match {
        case h :: t ⇒ Cons(h, g.value.from(t))
      }
    }
}

sealed trait NeoHList[H, L <: HList] extends OHList[H :: L] {
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

  def apply[H](h: H): Cons[H, HNil] = new Cons(h, OHList.nil)
  def apply[H, L <: HList](h: H, t: OHList[L]): Cons[H, L] = Cons(h, t)
  def apply[H1, H2, L <: HList](t: NeoHList[H2, L]): Extend[H1, H2, L] = Extend(t)

  case class Cons[H, L <: HList] (h: H, t: OHList[L]) extends NeoHList[H, L] with OHList.Cons[H, L] {
    def h_? = Some(h)
  }
  object Cons {
    def apply[H](h: H): Cons[H, HNil] = new Cons(h, OHList.nil)
//    def unapply[H, L <: HList](c: Cons[H, L]): Option[(H, OHList[L])] = Some((c.h, c.t))
  }
  case class Extend[H1, H2, L <: HList](t: NeoHList[H2, L]) extends NeoHList[H1, H2 :: L]

  implicit def genBase[H]: Generic.Aux[NeoHList[H, HNil], Cons[H, HNil] :+: CNil] =
    new Generic[NeoHList[H, HNil]] {
      type Repr = Cons[H, HNil] :+: CNil
      def to(t: NeoHList[H, HNil]): Repr = t match { case c: Cons[H, HNil] ⇒ Inl(c) }
      def from(r: Repr): NeoHList[H, HNil] = r match { case Inl(r) ⇒ r }
    }

  implicit def genCons[H1, H2, L <: HList]: Generic.Aux[NeoHList[H1, H2 :: L], Cons[H1, H2 :: L] :+: Extend[H1, H2, L] :+: CNil] =
    new Generic[NeoHList[H1, H2 :: L]] {
      type Repr = Cons[H1, H2 :: L] :+: Extend[H1, H2, L] :+: CNil
      def to(t: NeoHList[H1, H2 :: L]): Repr = t match {
        case l: Cons[H1, H2 :: L] ⇒ Inl(l)
        case l: Extend[H1, H2, L] ⇒ Inr(Inl(l))
      }
      def from(r: Repr): NeoHList[H1, H2 :: L] = r match {
        case Inl(cons) ⇒ cons
        case Inr(Inl(extend)) ⇒ extend
      }
    }
}
