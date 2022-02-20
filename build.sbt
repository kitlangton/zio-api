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
val zioSchemaVersion  = "0.2.0-RC1-1"

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
    name := "zio-api",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json"              % zioJsonVersion,
      "io.d11"  %% "zhttp"                 % zioHttpVersion,
      "dev.zio" %% "zio"                   % zioVersion,
      "dev.zio" %% "zio-schema"            % zioSchemaVersion,
      "dev.zio" %% "zio-schema-derivation" % zioSchemaVersion,
      "dev.zio" %% "zio-test"              % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(macros)

lazy val macros = (project in file("macros"))
  .settings(
    name := "zio-api-macros",
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
    name := "zio-api-examples",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json"   % zioJsonVersion,
      "io.d11"  %% "zhttp"      % zioHttpVersion,
      "dev.zio" %% "zio"        % zioVersion,
      "dev.zio" %% "zio-schema" % zioSchemaVersion,
      "dev.zio" %% "zio-test"   % zioVersion % Test
    )
  )
  .dependsOn(core)

lazy val coreTests = (project in file("coreTests"))
  .settings(
    name := "zio-api-core-tests",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json"     % zioJsonVersion,
      "io.d11"  %% "zhttp"        % zioHttpVersion,
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .dependsOn(core)

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    name := "zio-api-benchmarks",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-json" % zioJsonVersion,
      "io.d11"  %% "zhttp"    % zioHttpVersion,
      "dev.zio" %% "zio"      % zioVersion
    )
  )
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
