name := "CEO-AI"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "com.softwaremill.quicklens" %% "quicklens" % "1.4.11",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
)
