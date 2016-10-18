name := "abstractClass"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

wartremoverErrors ++= (Warts.unsafe.toSet).toSeq

wartremoverExcluded += baseDirectory.value / "src" / "test" / "scala" / "Test.scala"

