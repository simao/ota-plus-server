libraryDependencies ++= Seq(
  Dependencies.AkkaHttpTestKit,
  Dependencies.JsonWebSecurityAkka,
  Dependencies.SotaCore
) ++ Dependencies.JsonWebSecurity

dockerExposedPorts := Seq(8080)

dockerBaseImage := "java:openjdk-8-jre"

maintainer in Docker := "dev@advancedtelematic.com"

packageName in Docker := "ota-plus-core"

dockerRepository in Docker := Some("advancedtelematic")

dockerUpdateLatest in Docker := true

enablePlugins(SbtWeb, Versioning.Plugin)

Versioning.settings

Release.settings

enablePlugins(DockerPlugin, JavaAppPackaging)
