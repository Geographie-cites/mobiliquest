
val ScalaVersion = "2.13.7"
val Version = "0.1-SNAPSHOT"

val scalatraVersion = "2.7.1"
val jettyVersion = "11.0.2"
val json4sVersion = "3.6.11"
val autowireVersion = "0.3.3"
val boopickleVersion = "1.3.3"
val laminarVersion = "0.12.2"
val scaladgetVersion = "1.9.2-SNAPSHOT"
val scalajsDomVersion = "2.0.0"
val scalatagsVersion = "0.9.4"

lazy val shared = project.in(file("shared")) settings(
  scalaVersion := ScalaVersion
) enablePlugins (ScalaJSPlugin)

lazy val buildUI = taskKey[Unit]("builUI")

lazy val client = project.in(file("client")) enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin) settings(
  version := Version,
  scalaVersion := ScalaVersion,
  scalaJSUseMainModuleInitializer := false,
  webpackBundlingMode := BundlingMode.LibraryAndApplication(),
  //skip in packageJSDependencies := false,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "autowire" % autowireVersion,
    "io.suzaku" %%% "boopickle" % boopickleVersion,
    "com.raquo" %%% "laminar" % laminarVersion,
    "org.openmole.scaladget" %%% "tools" % scaladgetVersion,
    "org.openmole.scaladget" %%% "svg" % scaladgetVersion,
    "org.openmole.scaladget" %%% "bootstrapnative" % scaladgetVersion,
    "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
    "org.json4s" %% "json4s-jackson" % json4sVersion
  )
) dependsOn (shared)

lazy val server = project.in(file("server")) settings(
  name := "Mobiliquest",
  version := Version,
  scalaVersion := ScalaVersion,
  libraryDependencies ++= Seq(
    "org.ddahl" %% "rscala" % "3.2.19",
    "com.lihaoyi" %% "autowire" % autowireVersion,
    "io.suzaku" %% "boopickle" % boopickleVersion,
    "com.lihaoyi" %% "scalatags" % scalatagsVersion,
    "org.scalatra" %% "scalatra" % scalatraVersion,
    "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
    "javax.servlet" % "javax.servlet-api" % "4.0.1" % "provided",
    "org.eclipse.jetty" % "jetty-webapp" % jettyVersion,
    "org.eclipse.jetty" % "jetty-server" % jettyVersion
  )
) dependsOn (shared) enablePlugins (ScalatraPlugin)

lazy val bootstrap = project.in(file("target/bootstrap")) enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin) settings(
  version := Version,
  scalaVersion := ScalaVersion,
  buildUI := {
      val jsBuild = (client / Compile / fullOptJS / webpack).value.head.data

      val demoTarget = (server / Compile / target).value
      val demoResource = (client / Compile / resourceDirectory).value

      IO.copyFile(jsBuild, demoTarget / "webapp/js/demo.js")
      IO.copyDirectory(demoResource, demoTarget)
  }) dependsOn(client, server)
