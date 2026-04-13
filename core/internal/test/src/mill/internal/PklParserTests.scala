package mill.internal

import mill.api.Result
import mill.api.internal.HeaderData
import upickle.core.BufferedValue
import utest.*

object PklParserTests extends TestSuite {
  private val moduleSchema =
    """open module Module
      |
      |import "Trait.pkl"
      |
      |class MillBuild {
      |  repositories: Listing<String>?
      |}
      |
      |millVersion: String?
      |millBuild: MillBuild?
      |
      |moduleDeps: Listing<String>?
      |sources: Listing<String>?
      |
      |modules: Mapping<String, Module>?
      |traits: Listing<Trait>?
      |""".stripMargin

  private val traitSchema =
    """open module Trait
      |""".stripMargin

  private val scalaModuleSchema =
    """open module ScalaModule
      |
      |extends "../api/Module.pkl"
      |
      |scalaVersion: String?
      |""".stripMargin

  private val javaModuleSchema =
    """open module JavaModule
      |
      |extends "../api/Module.pkl"
      |
      |javacOptions: Listing<String>?
      |""".stripMargin

  private val kotlinModuleSchema =
    """open module KotlinModule
      |
      |extends "../javalib/JavaModule.pkl"
      |
      |kotlinVersion: String?
      |kotlinLanguageVersion: String?
      |kotlinApiVersion: String?
      |kotlincOptions: Listing<String>?
      |kotlincPluginMvnDeps: Listing<String>?
      |""".stripMargin

  private val testModuleSchema =
    """module TestModule
      |
      |import "../api/Trait.pkl"
      |
      |open class Junit5 extends Trait {
      |  junitPlatformVersion: String?
      |  jupiterVersion: String?
      |}
      |
      |class Junit6 extends Junit5 {
      |  jupiterVersion: String?
      |}
      |
      |class Specs2 extends Trait {
      |  specs2Version: String?
      |}
      |""".stripMargin

  private val spotlessModuleSchema =
    """module SpotlessModule
      |
      |extends "../../api/Trait.pkl"
      |
      |typealias RelPathRef = String
      |
      |class Suppress {
      |  path: String?
      |  step: String?
      |  shortCode: String?
      |}
      |
      |open class Step {}
      |
      |class GoogleJavaFormat extends Step {
      |  groupArtifact: String?
      |  version: String?
      |  style: String?
      |  formatJavadoc: Boolean?
      |}
      |
      |class Ktfmt extends Step {
      |  version: String?
      |  style: String?
      |  maxWidth: Int?
      |  blockIndent: Int?
      |  continuationIndent: Int?
      |  removeUnusedImports: Boolean?
      |  manageTrailingCommas: Boolean?
      |}
      |
      |class Format {
      |  steps: Listing<Step>
      |  includes: Listing<String>
      |  excludes: Listing<String>?
      |  lineEnding: String?
      |  encoding: String?
      |  suppressions: Listing<Suppress>?
      |}
      |
      |spotlessFormats: Listing<Format>?
      |spotlessExcludes: Listing<String>?
      |""".stripMargin

  private val scalafmtModuleSchema =
    """module ScalafmtModule
      |
      |extends "../../api/Trait.pkl"
      |
      |scalafmtConfig: Listing<String>?
      |""".stripMargin

  val tests = Tests {
    test("parseHeaderData maps schema module names and root keys") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/scalalib/ScalaModule.pkl"
          |
          |millVersion = "1.1.0"
          |scalaVersion = "3.7.4"
          |sources {}
          |millBuild {}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(headerData.`extends`.value.value.map(_.value) == Seq("mill.scalalib.ScalaModule"))
      assert(headerData.moduleDeps.value.value.isEmpty)
      assert(restValue(headerData, "mill-version") == BufferedValue.Str("1.1.0", 0))
      assert(restValue(headerData, "scalaVersion") == BufferedValue.Str("3.7.4", 0))
      assert(restValue(headerData, "sources") == BufferedValue.Arr(collection.mutable.ArrayBuffer.empty, 0))
      assert(restValue(headerData, "mill-build") == BufferedValue.Obj(collection.mutable.ArrayBuffer.empty, jsonableKeys = true, index = 0))
      assert(headerData.rest.keys.exists(_.value == "sources"))
      assert(!headerData.rest.keys.exists(_.value == "resources"))
    }

    test("parseHeaderData only renames legacy root keys") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/scalalib/ScalaModule.pkl"
          |
          |import "../schema/javalib/spotless/SpotlessModule.pkl"
          |
          |millVersion = "1.2.3"
          |scalaVersion = "3.7.4"
          |traits {
          |  new SpotlessModule {
          |    spotlessFormats {
          |      new SpotlessModule.Format {
          |        includes { "glob:**.scala" }
          |        steps {}
          |      }
          |    }
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(headerData.rest.keys.exists(_.value == "mill-version"))
      assert(!headerData.rest.keys.exists(_.value == "millVersion"))
      assert(headerData.rest.keys.exists(_.value == "scalaVersion"))
      assert(!headerData.rest.keys.exists(_.value == "scala-version"))
      assert(headerData.rest.keys.exists(_.value == "spotlessFormats"))
      assert(!headerData.rest.keys.exists(_.value == "spotless-formats"))
    }

    test("parseHeaderData converts nested modules recursively") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/scalalib/ScalaModule.pkl"
          |
          |import "../schema/javalib/JavaModule.pkl"
          |
          |modules {
          |  ["foo"] = new JavaModule {
          |    moduleDeps { "root" }
          |    javacOptions { "-Xlint" }
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      val nested = BufferedValue.transform(
        restValue(headerData, "object foo"),
        HeaderData.headerDataReader(buildFile)
      )

      assert(nested.`extends`.value.value.map(_.value) == Seq("mill.javalib.JavaModule"))
      assert(nested.moduleDeps.value.value.map(_.value) == Seq("root"))
      assert(restValue(nested, "javacOptions") ==
        BufferedValue.Arr(collection.mutable.ArrayBuffer(BufferedValue.Str("-Xlint", 0)), 0))
    }

    test("parseHeaderData maps KotlinModule and Kotlin-specific fields") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/kotlinlib/KotlinModule.pkl"
          |
          |kotlinVersion = "2.2.0"
          |kotlinLanguageVersion = "2.2"
          |kotlinApiVersion = "2.2"
          |kotlincOptions { "-Xcontext-receivers" }
          |kotlincPluginMvnDeps { "org.jetbrains.kotlin:kotlin-serialization-compiler-plugin:2.2.0" }
          |javacOptions { "-Xlint:deprecation" }
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(headerData.`extends`.value.value.map(_.value) == Seq("mill.kotlinlib.KotlinModule"))
      assert(restValue(headerData, "kotlinVersion") == BufferedValue.Str("2.2.0", 0))
      assert(restValue(headerData, "kotlinLanguageVersion") == BufferedValue.Str("2.2", 0))
      assert(restValue(headerData, "kotlinApiVersion") == BufferedValue.Str("2.2", 0))
      assert(restValue(headerData, "kotlincOptions") ==
        BufferedValue.Arr(collection.mutable.ArrayBuffer(BufferedValue.Str("-Xcontext-receivers", 0)), 0))
      assert(restValue(headerData, "kotlincPluginMvnDeps") ==
        BufferedValue.Arr(
          collection.mutable.ArrayBuffer(
            BufferedValue.Str("org.jetbrains.kotlin:kotlin-serialization-compiler-plugin:2.2.0", 0)
          ),
          0
        ))
      assert(restValue(headerData, "javacOptions") ==
        BufferedValue.Arr(
          collection.mutable.ArrayBuffer(BufferedValue.Str("-Xlint:deprecation", 0)),
          0
        ))
    }

    test("parseHeaderData lowers trait mixins into extends and flattens trait properties") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/javalib/JavaModule.pkl"
          |
          |import "../schema/javalib/TestModule.pkl"
          |
          |traits {
          |  new TestModule.Junit5 {
          |    junitPlatformVersion = "1.13.0"
          |    jupiterVersion = "5.13.0"
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.javalib.JavaModule", "mill.javalib.TestModule.Junit5")
      )
      assert(restValue(headerData, "junitPlatformVersion") == BufferedValue.Str("1.13.0", 0))
      assert(restValue(headerData, "jupiterVersion") == BufferedValue.Str("5.13.0", 0))
      assert(!headerData.rest.keys.exists(_.value == "traits"))
    }

    test("parseHeaderData supports Junit6 inheriting the Junit5 config surface") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/javalib/JavaModule.pkl"
          |
          |import "../schema/javalib/TestModule.pkl"
          |
          |traits {
          |  new TestModule.Junit6 {
          |    junitPlatformVersion = "6.0.0"
          |    jupiterVersion = "6.0.0"
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.javalib.JavaModule", "mill.javalib.TestModule.Junit6")
      )
      assert(restValue(headerData, "junitPlatformVersion") == BufferedValue.Str("6.0.0", 0))
      assert(restValue(headerData, "jupiterVersion") == BufferedValue.Str("6.0.0", 0))
    }

    test("parseHeaderData supports a second test-framework trait without JavaModule fields") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/scalalib/ScalaModule.pkl"
          |
          |import "../schema/javalib/TestModule.pkl"
          |
          |traits {
          |  new TestModule.Specs2 {
          |    specs2Version = "5.5.8"
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.scalalib.ScalaModule", "mill.javalib.TestModule.Specs2")
      )
      assert(restValue(headerData, "specs2Version") == BufferedValue.Str("5.5.8", 0))
      assert(!headerData.rest.keys.exists(_.value == "scalacOptions"))
    }

    test("parseHeaderData preserves nested Spotless trait configuration") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/javalib/JavaModule.pkl"
          |
          |import "../schema/javalib/spotless/SpotlessModule.pkl"
          |
          |traits {
          |  new SpotlessModule {
          |    spotlessExcludes { "glob:generated/**" }
          |    spotlessFormats {
          |      new SpotlessModule.Format {
          |        includes { "glob:**.java" }
          |        steps {
          |          new SpotlessModule.GoogleJavaFormat {
          |            version = "1.28.0"
          |            style = "GOOGLE"
          |          }
          |        }
          |        suppressions {
          |          new SpotlessModule.Suppress {
          |            step = "google-java-format"
          |          }
          |        }
          |      }
          |    }
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.javalib.JavaModule", "mill.javalib.spotless.SpotlessModule")
      )
      val spotlessFormats = restValue(headerData, "spotlessFormats")
      val formats = spotlessFormats match {
        case BufferedValue.Arr(items, _) => items
        case other => throw new java.lang.AssertionError(s"Expected array for spotlessFormats, got $other")
      }
      val firstFormat = formats.head match {
        case obj: BufferedValue.Obj => obj
        case other => throw new java.lang.AssertionError(s"Expected object for spotlessFormats element, got $other")
      }
      assert(objectField(firstFormat, "includes") ==
        BufferedValue.Arr(collection.mutable.ArrayBuffer(BufferedValue.Str("glob:**.java", 0)), 0))
      val steps = objectField(firstFormat, "steps") match {
        case BufferedValue.Arr(items, _) => items
        case other => throw new java.lang.AssertionError(s"Expected array for steps, got $other")
      }
      val firstStep = steps.head match {
        case obj: BufferedValue.Obj => obj
        case other => throw new java.lang.AssertionError(s"Expected object for step, got $other")
      }
      assert(objectField(firstStep, "version") == BufferedValue.Str("1.28.0", 0))
      assert(objectField(firstStep, "style") == BufferedValue.Str("GOOGLE", 0))
      assert(restValue(headerData, "spotlessExcludes") ==
        BufferedValue.Arr(collection.mutable.ArrayBuffer(BufferedValue.Str("glob:generated/**", 0)), 0))
    }

    test("parseHeaderData preserves Ktfmt Spotless configuration") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/kotlinlib/KotlinModule.pkl"
          |
          |import "../schema/javalib/spotless/SpotlessModule.pkl"
          |
          |traits {
          |  new SpotlessModule {
          |    spotlessFormats {
          |      new SpotlessModule.Format {
          |        includes { "glob:**.{kt,kts}" }
          |        steps {
          |          new SpotlessModule.Ktfmt {
          |            style = "GOOGLE_FORMAT"
          |            maxWidth = 100
          |            manageTrailingCommas = true
          |          }
          |        }
          |      }
          |    }
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.kotlinlib.KotlinModule", "mill.javalib.spotless.SpotlessModule")
      )
      val spotlessFormats = restValue(headerData, "spotlessFormats")
      val formats = spotlessFormats match {
        case BufferedValue.Arr(items, _) => items
        case other => throw new java.lang.AssertionError(s"Expected array for spotlessFormats, got $other")
      }
      val firstFormat = formats.head match {
        case obj: BufferedValue.Obj => obj
        case other => throw new java.lang.AssertionError(s"Expected object for spotlessFormats element, got $other")
      }
      assert(objectField(firstFormat, "includes") ==
        BufferedValue.Arr(collection.mutable.ArrayBuffer(BufferedValue.Str("glob:**.{kt,kts}", 0)), 0))
      val steps = objectField(firstFormat, "steps") match {
        case BufferedValue.Arr(items, _) => items
        case other => throw new java.lang.AssertionError(s"Expected array for steps, got $other")
      }
      val firstStep = steps.head match {
        case obj: BufferedValue.Obj => obj
        case other => throw new java.lang.AssertionError(s"Expected object for step, got $other")
      }
      assert(objectField(firstStep, "style") == BufferedValue.Str("GOOGLE_FORMAT", 0))
      assert(objectField(firstStep, "maxWidth") == BufferedValue.Int64(100, 0))
      assert(objectField(firstStep, "manageTrailingCommas") == BufferedValue.True(0))
    }

    test("parseHeaderData lowers ScalafmtModule and preserves scalafmtConfig") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../schema/scalalib/ScalaModule.pkl"
          |
          |import "../schema/scalalib/scalafmt/ScalafmtModule.pkl"
          |
          |traits {
          |  new ScalafmtModule {
          |    scalafmtConfig { ".scalafmt.conf" "project/.scalafmt.conf" }
          |  }
          |}
          |""".stripMargin,
        createFolders = true
      )

      val parsed = mill.internal.pkl.PklParser.parseHeaderData(buildFile)
      val headerData = parsed match {
        case Result.Success(value) => value
        case failure => throw new java.lang.AssertionError(failure.toString)
      }

      assert(
        headerData.`extends`.value.value.map(_.value) ==
          Seq("mill.scalalib.ScalaModule", "mill.scalalib.scalafmt.ScalafmtModule")
      )
      assert(restValue(headerData, "scalafmtConfig") ==
        BufferedValue.Arr(
          collection.mutable.ArrayBuffer(
            BufferedValue.Str(".scalafmt.conf", 0),
            BufferedValue.Str("project/.scalafmt.conf", 0)
          ),
          0
        ))
    }
  }

  private def writeSchema(workspace: os.Path): Unit = {
    val schemaDir = workspace / "schema"
    os.write.over(schemaDir / "api" / "Module.pkl", moduleSchema, createFolders = true)
    os.write.over(schemaDir / "api" / "Trait.pkl", traitSchema, createFolders = true)
    os.write.over(schemaDir / "scalalib" / "ScalaModule.pkl", scalaModuleSchema, createFolders = true)
    os.write.over(schemaDir / "javalib" / "JavaModule.pkl", javaModuleSchema, createFolders = true)
    os.write.over(schemaDir / "kotlinlib" / "KotlinModule.pkl", kotlinModuleSchema, createFolders = true)
    os.write.over(schemaDir / "javalib" / "TestModule.pkl", testModuleSchema, createFolders = true)
    os.write.over(
      schemaDir / "javalib" / "spotless" / "SpotlessModule.pkl",
      spotlessModuleSchema,
      createFolders = true
    )
    os.write.over(
      schemaDir / "scalalib" / "scalafmt" / "ScalafmtModule.pkl",
      scalafmtModuleSchema,
      createFolders = true
    )
  }

  private def restValue(headerData: HeaderData, key: String): BufferedValue =
    headerData.rest.collectFirst { case (locatedKey, value) if locatedKey.value == key => value }.get

  private def objectField(value: BufferedValue.Obj, key: String): BufferedValue =
    value match {
      case BufferedValue.Obj(entries, _, _) =>
        entries.collectFirst {
          case (BufferedValue.Str(found, _), fieldValue) if found == key => fieldValue
        }.get
    }
}
