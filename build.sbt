name := "STMPresentation"

description := "Scala STM Presentation"

organization in ThisBuild := "org.saegesser"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.2"

licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))

libraryDependencies ++= Seq(
  "org.scala-stm" %% "scala-stm" % "0.7"
)

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-Yno-adapted-args",
  // "-Ywarn-value-discard",
  "-Ywarn-numeric-widen",
  "-Ywarn-dead-code",
  // "-Xfatal-warnings",
  "-Xlint",
 "-unchecked"
)

initialCommands in console := """
import stmdemo._
import scala.concurrent._
import scala.concurrent.stm._
import ExecutionContext.Implicits.global"""

// fork in run := true

// javaOptions in run := Seq("-Dccstm.stats=true")

lazy val core = project.in(file("."))
