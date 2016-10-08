name := "list"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"

wartremoverErrors ++= (Warts.unsafe.toSet).toSeq

wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "Test.scala"

