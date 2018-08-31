enablePlugins(TomcatPlugin)

scalaVersion in ThisBuild := "2.12.6"

lazy val liftScalaVersion = "2.12"
lazy val liftVersion = "3.2.0"
lazy val logbackVersion = "1.2.3"
lazy val slf4jVersion = "1.7.25"
lazy val mysqlConnectorVersion = "5.1.46"
lazy val servletArtifact = "javax.servlet-api"
lazy val servletVersion = "3.0.1"

lazy val commonSettings = Seq(
    cancelable := true,

    developers := List(
      Developer("hep",
                "Howard Palmer",
                "hep@acm.org",
                url("https://github.com/hpalmer")
      )
    ),

    // define the statements initially evaluated when entering 'console', 'console-quick', but not 'console-project'
    initialCommands in console := """
                                    |""".stripMargin,

    javacOptions ++= Seq(
      "-Xlint:deprecation",
      "-Xlint:unchecked",
      "-source", "1.8",
      "-target", "1.8",
      "-g:vars"
    )
)

lazy val choice = (project in file("."))
    .settings(
        commonSettings,

        artifactName := { (v: ScalaVersion, m: ModuleID, a: Artifact) =>
            a.name + "." + a.extension
        },

        libraryDependencies ++= Seq(
            "net.liftweb" %  s"lift-common_$liftScalaVersion" % liftVersion,
            "net.liftweb" %  s"lift-mapper_$liftScalaVersion" % liftVersion,
            "net.liftweb" %  s"lift-json_$liftScalaVersion" % liftVersion,
            "net.tanesha.recaptcha4j" %  "recaptcha4j" % "0.0.7",
            "org.jasypt" %  "jasypt" % "1.9.2" % "compile",
            "org.slf4j" %  "slf4j-api" % slf4jVersion,
            "ch.qos.logback" %  "logback-classic" % logbackVersion,
            "org.luaj" %  "luaj-jse" % "3.0.1",
            "com.h2database" %  "h2" % "1.4.197" % "runtime",
            "net.bull.javamelody" %  "javamelody-core" % "1.73.1",
            "javax.servlet" %  servletArtifact % servletVersion % "provided",
            "org.junit.jupiter" %  "junit-jupiter-api" % "5.2.0" % "test",
            "org.specs2" %  s"specs2-core_$liftScalaVersion" % "4.3.1" % "test",
            "org.eclipse.jetty" %  "jetty-server" % "9.4.11.v20180605" % "test",
            "org.eclipse.jetty" %  "jetty-webapp" % "9.4.11.v20180605" % "test",
            "org.scala-lang" %  "scala-compiler" % scalaVersion.toString,
            "mysql" %  "mysql-connector-java" % mysqlConnectorVersion % "provided",
            "org.apache.tika" %  "tika-core" % "1.18" % "compile",
            "org.scalatest"     %% "scalatest"   % "3.0.3" % Test withSources(),
            "junit"             %  "junit"       % "4.12"  % Test
        ),

        // If you want to apply a license, such as the Apache 2 license, uncomment the following:
        licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),

        logLevel := Level.Warn,

        // Only show warnings and errors on the screen for compilations.
        // This applies to both test:compile and compile and is Info by default
        logLevel in compile := Level.Warn,

        // Level.INFO is needed to see detailed output when running tests
        logLevel in test := Level.Info,

        name := "choice-server",

        organization := "edu.stanford.aaalab",

        resolvers ++= Seq(
        ),

        scalacOptions ++= Seq( // From https://tpolecat.github.io/2017/04/25/scalac-flags.html
          "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
          "-encoding", "utf-8",                // Specify character encoding used by source files.
          "-explaintypes",                     // Explain type errors in more detail.
          "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
          "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
          "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
          "-language:higherKinds",             // Allow higher-kinded types
          "-language:implicitConversions",     // Allow definition of implicit functions called views
          "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
          "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
          //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
          "-Xfuture",                          // Turn on future language features.
          "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
          "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
          "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
          "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
          "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
          "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
          "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
          "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
          "-Xlint:option-implicit",            // Option.apply used implicit view.
          "-Xlint:package-object-classes",     // Class or object defined in package object.
          "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
          "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
          "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
          "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
          "-Xlint:unsound-match",              // Pattern match may not be typesafe.
          "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification",             // Enable partial unification in type constructor inference
          //"-Ywarn-dead-code",                  // Warn when dead code is identified.
          "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
          "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
          "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
          "-Ywarn-numeric-widen"               // Warn when numerics are widened.
          //"-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
          //"-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
          //"-Ywarn-unused:locals",              // Warn if a local definition is unused.
          //"-Ywarn-unused:params",              // Warn if a value parameter is unused.
          //"-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
          //"-Ywarn-unused:privates",            // Warn if a private member is unused.
          //"-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
        ),

        // The REPL can’t cope with -Ywarn-unused:imports or -Xfatal-warnings so turn them off for the console
        scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),

        scalacOptions in (Compile, doc) ++= baseDirectory.map {
          bd: File ⇒ Seq[String](
             "-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/hpalmer/choice-server/tree/master€{FILE_PATH}.scala"
          )
        }.value,

        scmInfo := Some(
          ScmInfo( 
            url(s"https://github.com/hpalmer/$name"),
            s"git@github.com:hpalmer/$name.git"
          )
        ),

        version := "3.0.0"

    )
