import com.banno.license.Licenses._
import com.banno.license.Plugin.LicenseKeys._
import com.banno.license.Plugin._

name := "scala-phases-chain"

version := "0.0.1"

licenseSettings

license := apache2("Copyright 2015 the original author or authors.")

scalaVersion := "2.11.5"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % "test"


