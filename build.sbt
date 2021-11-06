ThisBuild / scalaVersion     := "2.13.6"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val sttpVersion       = "3.3.13"
val zioHttpVersion    = "1.0.0.0-RC17"
val zioJsonVersion    = "0.1.5"
val zioMagicVersion   = "0.3.8"
val zioNioVersion     = "1.0.0-RC11"
val zioProcessVersion = "0.5.0"
val zioVersion        = "1.0.12"
val zioSchemaVersion  = "0.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "zio-routes",
    libraryDependencies ++= Seq(
      "io.d11"  %% "zhttp"      % zioHttpVersion,
      "dev.zio" %% "zio"        % zioVersion,
      "dev.zio" %% "zio-schema" % zioSchemaVersion,
      "dev.zio" %% "zio-test"   % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
