lazy val beginner = project in file("beginner")

lazy val javadev = project in file("javadev")

lazy val sparkdev = (project in file("sparkdev"))
  .settings(
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "org.apache.spark" %% "spark-core" % "2.2.0"
    )
  )

lazy val freemonad = project in file("freemonad")

lazy val taglessfinal = project in file("taglessfinal")

lazy val freemonadfreek = (project in file("freemonadfreek")).settings(
  libraryDependencies ++= Seq(
    "com.projectseptember" %% "freek" % "0.6.5"
  ),
  scalacOptions := Seq("-Ypartial-unification"),
  resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")
)

lazy val almostfp = (project in file("almostfp")).settings(
  libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"
)

lazy val cake = project in file("cake")

lazy val johndegoes = (project in file("johndegoes")).settings(
  libraryDependencies += "org.scalaz" %% "scalaz-zio" % "1.0-RC4"
)

lazy val akkadev = (project in file("akkadev")).settings(
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.23"
)

lazy val trampoline = project in file("trampoline")

lazy val peanoadt = project in file("peanoadt")

lazy val shapeless = (project in file("shapeless")).settings(
  libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
)

lazy val peanotypes = project in file("peanotypes")

lazy val apofutuhylo = project in file("apofutuhylo")

lazy val paramorphism = project in file("paramorphism")

lazy val fixcombinator = project in file("fixcombinator")

lazy val root = (project in file("."))
  .aggregate(
    beginner,
    almostfp,
    freemonad,
    freemonadfreek,
    taglessfinal,
    cake,
    johndegoes,
    sparkdev,
    akkadev,
    trampoline,
    peanoadt,
    shapeless,
    peanotypes,
    apofutuhylo,
    paramorphism,
    fixcombinator
  )
