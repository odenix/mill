package mill.internal

import mill.api.Result
import mill.api.internal.HeaderData
import upickle.core.BufferedValue
import utest.*

object PklParserTests extends TestSuite {
  private val moduleSchema =
    """open module Module
      |
      |import "Module.pkl"
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
      |""".stripMargin

  private val scalaModuleSchema =
    """open module ScalaModule
      |
      |extends "Module.pkl"
      |
      |scalaVersion: String?
      |""".stripMargin

  private val javaModuleSchema =
    """open module JavaModule
      |
      |extends "Module.pkl"
      |
      |javacOptions: Listing<String>?
      |""".stripMargin

  private val kotlinModuleSchema =
    """open module KotlinModule
      |
      |extends "JavaModule.pkl"
      |
      |kotlinVersion: String?
      |kotlinLanguageVersion: String?
      |kotlinApiVersion: String?
      |kotlincOptions: Listing<String>?
      |kotlincPluginMvnDeps: Listing<String>?
      |""".stripMargin

  val tests = Tests {
    test("parseHeaderData maps schema module names and root keys") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../pkl/ScalaModule.pkl"
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

    test("parseHeaderData converts nested modules recursively") {
      val workspace = os.temp.dir(prefix = "mill-pkl-parser-")
      writeSchema(workspace)
      val buildFile = workspace / "work" / "build.pkl"
      os.write.over(
        buildFile,
        """amends "../pkl/ScalaModule.pkl"
          |
          |import "../pkl/JavaModule.pkl"
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
        """amends "../pkl/KotlinModule.pkl"
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
  }

  private def writeSchema(workspace: os.Path): Unit = {
    val schemaDir = workspace / "pkl"
    os.write.over(schemaDir / "Module.pkl", moduleSchema, createFolders = true)
    os.write.over(schemaDir / "ScalaModule.pkl", scalaModuleSchema, createFolders = true)
    os.write.over(schemaDir / "JavaModule.pkl", javaModuleSchema, createFolders = true)
    os.write.over(schemaDir / "KotlinModule.pkl", kotlinModuleSchema, createFolders = true)
  }

  private def restValue(headerData: HeaderData, key: String): BufferedValue =
    headerData.rest.collectFirst { case (locatedKey, value) if locatedKey.value == key => value }.get
}
