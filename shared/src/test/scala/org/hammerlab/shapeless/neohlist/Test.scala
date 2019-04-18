package org.hammerlab.shapeless.neohlist

import shapeless._

class Test
  extends hammerlab.Suite {
  test("generics") {
    import NeoHList.{ Cons, Extend }
    !![Generic[Cons[String, HNil]]]
    !![Generic[Extend[String, Int, HNil]]]
    !![Generic[NeoHList[String, Int :: HNil]]]
  }
}
