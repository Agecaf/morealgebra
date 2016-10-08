scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.11.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

scalacOptions in (Compile, doc) ++= Seq("-doc-root-content", baseDirectory.value+"/root-doc.scala")

target in Compile in doc := baseDirectory.value / "docs"
