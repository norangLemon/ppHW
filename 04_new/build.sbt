scalaVersion := "2.11.8"

name := "subtype_new"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

wartremoverErrors ++= (Warts.unsafe.toSet).toSeq

wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "Test.scala"

