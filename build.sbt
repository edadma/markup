name := "Markup"

version := "0.4"

scalaVersion := "2.11.2"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "org.lteditor"

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-swing" % "1.0.1",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
	)

resolvers += "Hyperreal Repository" at "http://hyperreal.ca/maven2"

libraryDependencies ++= Seq(
	"org.lteditor" %% "typesetter" % "0.4"
	)

publishTo := Some( Resolver.sftp( "private", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("GPL" -> url("http://opensource.org/licenses/GPL-3.0"))

homepage := Some(url("https://github.com/LTEditor/markup"))

pomExtra := (
  <scm>
    <url>git@github.com:LTEditor/markup.git</url>
    <connection>scm:git:git@github.com:LTEditor/markup.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://lteditor.org</url>
    </developer>
  </developers>)
