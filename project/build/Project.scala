import sbt._
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  lazy val scalap = "org.scala-lang" % "scalap" % "2.9.0" withSources()
}