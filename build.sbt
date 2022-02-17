ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val sttpVersion       = "3.4.1"
val zioHttpVersion    = "2.0.0-RC3"
val zioJsonVersion    = "0.3.0-RC3"
val zioMagicVersion   = "0.3.8"
val zioNioVersion     = "2.0.0-RC2"
val zioProcessVersion = "0.7.0-RC2-2"
val zioVersion        = "2.0.0-RC2"
val zioSchemaVersion  = "0.1.7"

Global / onChangedBuildSource := ReloadOnSourceChanges

val sharedSettings = Seq(
  addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
//  scalacOptions ++= Seq("-Xfatal-warnings"),
  resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype OSS Snapshots s01" at "https://s01.oss.sonatype.org/content/repositories/snapshots"
  ),
  scalacOptions ++= Seq("-Ymacro-annotations", "-deprecation", "-feature"),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

lazy val root = (project in file("."))
  .aggregate(core, macros, examples)

lazy val core = (project in file("core"))
  .settings(
    name := "zio-routes",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json"   % zioJsonVersion,
      "io.d11"  %% "zhttp"      % zioHttpVersion,
      "dev.zio" %% "zio"        % zioVersion,
      "dev.zio" %% "zio-schema" % zioSchemaVersion,
      "dev.zio" %% "zio-test"   % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(
    name := "zio-routes-macros",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json"   % zioJsonVersion,
      "io.d11"  %% "zhttp"      % zioHttpVersion,
      "dev.zio" %% "zio"        % zioVersion,
      "dev.zio" %% "zio-schema" % zioSchemaVersion,
      "dev.zio" %% "zio-test"   % zioVersion % Test,
      // scala macro dependencies
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val examples = (project in file("examples"))
  .settings(
    name := "zio-routes-examples",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.github.kitlangton" %% "zio-magic"  % "0.3.9",
      "dev.zio"              %% "zio-json"   % zioJsonVersion,
      "io.d11"               %% "zhttp"      % zioHttpVersion,
      "dev.zio"              %% "zio"        % zioVersion,
      "dev.zio"              %% "zio-schema" % zioSchemaVersion,
      "dev.zio"              %% "zio-test"   % zioVersion % Test
    )
  )
  .dependsOn(core)
