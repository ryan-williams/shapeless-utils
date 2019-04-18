lazy val `shapeless-utils` =
  cross
    .in(file("."))
    .settings(
      v"1.6.0",
      dep(
        cats,
        shapeless
      )
    )
lazy val `shapeless-utils-root` =
  `shapeless-utils`.x
    .settings(
      github.repo("shapeless-utils")
    )
