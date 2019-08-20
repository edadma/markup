name := "markup"

version := "0.4"

scalaVersion := "2.13.0"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-swing" % "2.1.1",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
	)

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"xyz.hyperreal" %% "typesetter" % "0.5"
	)

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/markup"))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/markup.git</url>
    <connection>scm:git:git@github.com:edadma/markup.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://lteditor.org</url>
    </developer>
  </developers>