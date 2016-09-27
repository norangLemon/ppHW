name := "recursion"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"

wartremoverErrors ++= (Warts.unsafe.toSet - Wart.ListOps).toSeq

wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "Test.scala"

