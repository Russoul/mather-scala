// The simplest possible sbt build file is just one line:

scalaVersion := "0.5.0-RC1"
name := "mather"
organization := "me.russoul"
version := "1.0"

libraryDependencies += ("org.typelevel" %% "cats" % "0.9.0").withDottyCompat()


