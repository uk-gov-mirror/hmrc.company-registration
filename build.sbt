import sbt.Keys.{javaOptions, parallelExecution, *}
import sbt.*
import uk.gov.hmrc.DefaultBuildSettings.*
import uk.gov.hmrc.DefaultBuildSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

val appName = "company-registration"

ThisBuild / majorVersion := 1
ThisBuild / scalaVersion := "2.13.18"

lazy val scoverageSettings = {
  // Semicolon-separated list of regexs matching classes to exclude
  import scoverage.ScoverageKeys
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;Reverse.*;model.*;config.*;.*(AuthService|BuildInfo|Routes).*",
    ScoverageKeys.coverageMinimumStmtTotal := 90,
    ScoverageKeys.coverageFailOnMinimum    := true,
    ScoverageKeys.coverageHighlighting     := true
  )
}

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin) // Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(PlayKeys.playDefaultPort := 9973)
  .settings(scalaSettings *)
  .settings(scoverageSettings *)
  .settings(defaultSettings() *)
  .settings(
    scalacOptions += "-Xlint:-unused",
    targetJvm := "jvm-1.8",
    libraryDependencies ++= AppDependencies(),
    Test / parallelExecution := false,
    Test / fork              := false,
    retrieveManaged          := true,
    scalacOptions ++= List("-Xlint:-missing-interpolator")
  )

lazy val it = project
  .enablePlugins(PlayScala)
  .dependsOn(microservice % "test->test") // the "test->test" allows reusing test code and test dependencies
  .settings(DefaultBuildSettings.itSettings())
  .settings(libraryDependencies ++= AppDependencies.test)

Test / javaOptions += "-Dlogger.resource=logback-test.xml"
