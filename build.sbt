
val ScalaVersion = "3.2.0"
val Version = "0.1-SNAPSHOT"

val laminarVersion = "0.14.5"
val scaladgetVersion = "1.9.2"
val scalajsDomVersion = "2.0.0"
val scalatagsVersion = "0.11.1"
val betterFilesVersion = "3.9.1"
val minioVersion = "8.4.2"
val json4sVersion = "4.0.6"
val http4sVersion = "0.23.13"

val endpoints4SVersion = "1.8.0+n"
val endpointCirceVersion = "2.2.0+n"

lazy val shared = project.in(file("shared")) settings(
  scalaVersion := ScalaVersion,
  libraryDependencies ++= Seq(
    "org.openmole.endpoints4s" %%% "algebra" % endpoints4SVersion,
    "org.openmole.endpoints4s" %%% "json-schema-circe" % endpointCirceVersion,
    "io.circe" %% "circe-generic" % "0.14.3"
  )
) enablePlugins (ScalaJSPlugin)

lazy val buildUI = taskKey[Unit]("builUI")

lazy val client = project.in(file("client")) enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin) settings(
  version := Version,
  scalaVersion := ScalaVersion,
  scalaJSUseMainModuleInitializer := false,
  webpackBundlingMode := BundlingMode.LibraryAndApplication(),
  scalaJSLinkerConfig := scalaJSLinkerConfig.value.withSourceMap(false),
  webpackNodeArgs := Seq("--openssl-legacy-provider"),
  //skip in packageJSDependencies := false,
  libraryDependencies ++= Seq(
    "com.raquo" %%% "laminar" % laminarVersion,
    "org.openmole.scaladget" %%% "tools" % scaladgetVersion,
    "org.openmole.scaladget" %%% "svg" % scaladgetVersion,
    "org.openmole.scaladget" %%% "bootstrapnative" % scaladgetVersion,
    "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
    "org.openmole.endpoints4s" %%% "xhr-client" % "5.1.0+n"
  )
) dependsOn (shared)


lazy val server = project.in(file("server")) enablePlugins(DockerPlugin, JavaAppPackaging) settings(
  name := "Mobiliquest",
  version := Version,
  scalaVersion := ScalaVersion,
  dockerExposedPorts ++= Seq(8080),
  libraryDependencies ++= Seq(
    "org.ddahl" % "rscala_2.13" % "3.2.19",
    "com.lihaoyi" %% "scalatags" % scalatagsVersion,
    "org.openmole.endpoints4s" %% "http4s-server" % "10.0.0+n",
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.json4s" %% "json4s-jackson" % json4sVersion,
    "io.circe" %% "circe-parser" % "0.14.3",
    "com.github.pathikrit" % "better-files-akka_2.13" % betterFilesVersion,
    "io.minio" % "minio" % minioVersion
  ),
  Compile / compile := {
    val jsBuild = (client / Compile / fastOptJS / webpack).value.head.data

    val demoTarget = (Compile / target).value
    val demoResource = (client / Compile / resourceDirectory).value

    IO.copyFile(jsBuild, demoTarget / "webapp/js/demo.js")
    IO.copyDirectory(demoResource, demoTarget)
    (Compile / compile).value
  }
) dependsOn (shared)

//lazy val bootstrap = project.in(file("target/bootstrap")) enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin) settings(
//  version := Version,
//  scalaVersion := ScalaVersion,
//  buildUI := {
//    val jsBuild = (client / Compile / fastOptJS / webpack).value.head.data
//
//    val demoTarget = (server / Compile / target).value
//    val demoResource = (client / Compile / resourceDirectory).value
//
//    IO.copyFile(jsBuild, demoTarget / "webapp/js/demo.js")
//    IO.copyDirectory(demoResource, demoTarget)
//  }) dependsOn(client, server)
