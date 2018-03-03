name := "CEO-AI"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  //  "-Ywarn-value-discard",
  //  "-Ywarn-unused"
)

libraryDependencies ++= Seq(
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
)
