val swamCommit = "39999a1751076c6dbfe2a92c874f17683730d14e"
val swam = uri(s"https://gitlab.rlp.net/plmz/external/swam.git#$swamCommit")
val sturdyCommit = "647e12ddf7fab14a23a4ffbc0fd4d292f127e7dd"
val sturdy = uri(s"https://gitlab.rlp.net/plmz/sturdy.scala.git#$sturdyCommit")

lazy val root = (project in file("."))
  .dependsOn(ProjectRef(swam, "swam_core") % "compile->compile;test->test")
  .dependsOn(ProjectRef(swam, "swam_text") % "compile->compile;test->test")
  .dependsOn(ProjectRef(sturdy, "sturdy_wasm") % "compile->compile;test->test")
  .settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "3.2.1"
    )),
    name := "wasm-sturdopt",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    )
  )