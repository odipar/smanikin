organization := "org.jmanikin"
name := "jmanikin-scala"

version := "0.2"

scalaVersion := "2.13.3"

resolvers += Resolver.mavenLocal

libraryDependencies += "org.jmanikin" % "jmanikin-core" % "0.2"
libraryDependencies += "com.twitter" %% "chill" % "0.9.5"
libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.2"
libraryDependencies += "com.typesafe.slick" %% "slick-codegen" % "3.3.2"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.25"
libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % "3.3.2"
libraryDependencies += "org.postgresql" % "postgresql" % "42.2.14.jre7"
libraryDependencies += "com.h2database" % "h2" % "1.4.200"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
