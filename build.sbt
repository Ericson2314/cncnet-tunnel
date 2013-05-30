name := "CnCNet Tunnel"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.2"

resolvers += Resolver.url(
  "sbt-plugin-releases",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")