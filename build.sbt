val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Twirl Semantic Tokens",
		organization := "org",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= List(
			"org.playframework.twirl" %% "twirl-parser" 			% "2.1.0-M8",			
			"org.eclipse.lsp4j"				 % "org.eclipse.lsp4j"  % "1.0.0"
		)
  )
