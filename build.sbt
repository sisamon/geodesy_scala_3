//ThisBuild / version := "0.1.0-SNAPSHOT"
//ThisBuild / scalaVersion := "3.3.3"
//
//val neotypeVersion = "0.3.0"
//
//lazy val root = (project in file("."))
//  .settings(
//    name := "capex_optimization",
//    idePackagePrefix := Some("org.a3.capex"),
//    libraryDependencies ++= Seq(
//      //core
//        "io.github.kitlangton" %% "neotype" % "0.3.0"
//    )
//  )

version := "0.1.0-SNAPSHOT"
scalaVersion := "3.3.0"
name := "geodesic_scala"

scalacOptions ++= Seq("-deprecation", "-encoding", "UTF-8", "-feature", "-unchecked", "-language:implicitConversions")

val neotypeVersion = "0.3.0"

idePackagePrefix := Some("org.a3.capex")
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies ++= Seq(
    //core
    "dev.soundness" % "quantitative-units" % "0.3.0",
    "org.typelevel" %% "squants" % "1.8.3",
    "io.github.kitlangton"  %%  "neotype"  % "0.3.0",
    "dev.zio"  %%  "zio-prelude"  % "1.0.0-RC27",
    "org.scalaz"  %%  "scalaz-core"  % "7.3.8",
    "org.typelevel"  %%  "spire"  % "0.18.0",
    "com.github.pureconfig" %% "pureconfig-core" % "0.17.7",
)
