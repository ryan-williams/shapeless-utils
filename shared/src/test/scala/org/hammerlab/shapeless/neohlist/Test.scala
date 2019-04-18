package org.hammerlab.shapeless.neohlist

import shapeless._

sealed trait I[H, L <: HList]
object I {
  //type I[H, L <: HList]
  case class A[H, L <: HList](h: H, t: OHList[L]) extends I[H, L]
  //def extend[H1, H2, L <: HList]: I[H1, H2 :: L]
  case class B[H1, H2, L <: HList](t: I[H2, L]) extends I[H1, H2 :: L]

  implicit def genA[H]: Generic.Aux[I[H, HNil], A[H, HNil] :+: CNil] =
    new Generic[I[H, HNil]] {
      type Repr = A[H, HNil] :+: CNil
      def to(t: I[H, HNil]): Repr = t match {
        case a: A[H, HNil] ⇒ Inl(a)
      }
      def from(r: Repr): I[H, HNil] =
        r match {
          case Inl(a) ⇒ a
        }
    }
  implicit def genB[H1, H2, L <: HList]: Generic.Aux[I[H1, H2 :: L], A[H1, H2 :: L] :+: B[H1, H2, L] :+: CNil] =
    new Generic[I[H1, H2 :: L]] {
      type Repr = A[H1, H2 :: L] :+: B[H1, H2, L] :+: CNil
      override def to(t: I[H1, H2 :: L]): Repr = t match {
        case a: A[H1, H2 :: L] ⇒ Inl(a)
        case b: B[H1, H2, L] ⇒ Inr(Inl(b))
      }
      override def from(r: Repr): I[H1, H2 :: L] = r match {
        case Inl(a) ⇒ a
        case Inr(Inl(b)) ⇒ b
      }
    }
}

class Test
  extends hammerlab.Suite {
  test("generics") {
    import I.{A, B}
    !![Generic[A[String, HNil]]]
    !![Generic[B[String, Int, HNil]]]
    !![Generic[I[String, HNil]]]
    !![Generic[I[String, Int :: HNil]]]

    import NeoHList.{ Cons, Extend }
    !![Generic[Cons[String, HNil]]]
    !![Generic[Extend[String, Int, HNil]]]
    !![Generic[NeoHList[String, Int :: HNil]]]
    !![Generic[NeoHList[String, HNil]]]

    !![Generic[OHList[HNil]]]
    !![Generic[OHList[String :: HNil]]]

//    !![Generic[String :: Int :: HNil]]
//    !![Generic[String :: OHList[HNil] :: HNil]]
  }
}
