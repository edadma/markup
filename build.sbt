name := "Markup"

version := "0.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "org.lteditor"

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-swing" % "1.0.1",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
	)

libraryDependencies ++= Seq(
	"org.lteditor" %% "typesetter" % "0.1"
	)
