package mill.internal.pkl

import mill.api.Result
import mill.api.internal.{Appendable, HeaderData, Located, OneOrMore}
import upickle.core.BufferedValue

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

object PklParser {
  private val extendsMapping = Map(
    "Module" -> "Module",
    "ScalaModule" -> "mill.scalalib.ScalaModule",
    "JavaModule" -> "mill.javalib.JavaModule"
  )

  private val depFieldNames = Set(
    "moduleDeps",
    "compileModuleDeps",
    "runModuleDeps",
    "bomModuleDeps"
  )

  private val rootKeyMapping = Map(
    "millVersion" -> "mill-version",
    "millJvmVersion" -> "mill-jvm-version",
    "millJvmIndexVersion" -> "mill-jvm-index-version",
    "millJvmOpts" -> "mill-jvm-opts",
    "millOpts" -> "mill-opts",
    "millAllowNestedBuildMill" -> "mill-allow-nested-build-mill",
    "millRepositories" -> "mill-repositories",
    "millBuild" -> "mill-build"
  )

  def parseHeaderData(scriptFile: os.Path): Result[HeaderData] = {
    try {
      import org.pkl.core.{Evaluator, ModuleSource}

      val module =
        Using.resource(Evaluator.preconfigured()) { evaluator =>
          evaluator.evaluate(ModuleSource.path(scriptFile.toNIO))
        }

      Result.Success(convertModule(scriptFile, module))
    } catch {
      case e: org.pkl.core.PklException =>
        Result.Failure(s"Failed parsing ${scriptFile.last}: ${e.getMessage}")
    }
  }

  private def convertModule(scriptFile: os.Path, module: org.pkl.core.PModule): HeaderData =
    convertModule(scriptFile, module.getClassInfo.getModuleName, module.getProperties.asScala.toMap)

  private def convertModule(
      scriptFile: os.Path,
      moduleName: String,
      props: Map[String, Any]
  ): HeaderData = {
    val extendsValue = extendsMapping.get(moduleName).toSeq.map(Located(scriptFile, 0, _))

    HeaderData(
      `extends` = Located(scriptFile, 0, OneOrMore(extendsValue)),
      moduleDeps = Located(scriptFile, 0, Appendable(readDeps(scriptFile, props.get("moduleDeps")))),
      compileModuleDeps =
        Located(scriptFile, 0, Appendable(readDeps(scriptFile, props.get("compileModuleDeps")))),
      runModuleDeps = Located(scriptFile, 0, Appendable(readDeps(scriptFile, props.get("runModuleDeps")))),
      bomModuleDeps = Located(scriptFile, 0, Appendable(readDeps(scriptFile, props.get("bomModuleDeps")))),
      rest = collectRest(scriptFile, props)
    )
  }

  private def readDeps(scriptFile: os.Path, valueOpt: Option[Any]): Seq[Located[String]] =
    valueOpt match {
      case Some(xs: java.util.List[?]) =>
        xs.asScala.collect { case s: String => Located(scriptFile, 0, s: String) }.toSeq
      case Some(xs: Seq[?]) =>
        xs.collect { case s: String => Located(scriptFile, 0, s: String) }
      case _ => Nil
    }

  private def collectRest(
      scriptFile: os.Path,
      props: Map[String, Any]
  ): Map[Located[String], BufferedValue] = {
    val builder = Map.newBuilder[Located[String], BufferedValue]
    props.iterator.foreach {
      case (name, _) if depFieldNames(name) => ()
      case ("modules", value) =>
        builder ++= convertModules(scriptFile, value)
      case (name, value) =>
        toBufferedValue(scriptFile, value).foreach { buffered =>
          builder += Located(scriptFile, 0, rootKeyMapping.getOrElse(name, name)) -> buffered
        }
    }
    builder.result()
  }

  private def convertModules(
      scriptFile: os.Path,
      value: Any
  ): Iterable[(Located[String], BufferedValue)] = value match {
    case mapping: java.util.Map[?, ?] =>
      mapping.asScala.iterator.collect {
        case (name: String, module: org.pkl.core.PModule) =>
          Located(scriptFile, 0, s"object $name") -> headerDataToBufferedValue(convertModule(scriptFile, module))
        case (name: String, obj: org.pkl.core.PObject) =>
          Located(scriptFile, 0, s"object $name") -> headerDataToBufferedValue(
            convertModule(scriptFile, obj.getClassInfo.getModuleName, obj.getProperties.asScala.toMap)
          )
      }.toSeq
    case _ => Seq.empty
  }

  private[pkl] def headerDataToBufferedValue(headerData: HeaderData): BufferedValue = {
    val entries = mutable.ArrayBuffer.empty[(BufferedValue, BufferedValue)]
    if (headerData.`extends`.value.value.nonEmpty) {
      entries += (
        (BufferedValue.Str("extends", 0): BufferedValue) ->
          seqToBufferedValue(headerData.`extends`.value.value.map(_.value))
      )
    }
    appendDeps(entries, "moduleDeps", headerData.moduleDeps.value.value)
    appendDeps(entries, "compileModuleDeps", headerData.compileModuleDeps.value.value)
    appendDeps(entries, "runModuleDeps", headerData.runModuleDeps.value.value)
    appendDeps(entries, "bomModuleDeps", headerData.bomModuleDeps.value.value)
    for ((key, value) <- headerData.rest.toSeq) {
      entries += (((BufferedValue.Str(key.value, key.index): BufferedValue), value))
    }
    BufferedValue.Obj(entries, jsonableKeys = true, index = 0)
  }

  private def appendDeps(
      entries: mutable.ArrayBuffer[(BufferedValue, BufferedValue)],
      key: String,
      deps: Seq[Located[String]]
  ): Unit = {
    if (deps.nonEmpty) {
      entries += ((BufferedValue.Str(key, 0): BufferedValue) -> seqToBufferedValue(deps.map(_.value)))
    }
  }

  private def seqToBufferedValue(values: Seq[String]): BufferedValue =
    BufferedValue.Arr(values.iterator.map(BufferedValue.Str(_, 0)).to(mutable.ArrayBuffer), 0)

  private def toBufferedValue(scriptFile: os.Path, value: Any): Option[BufferedValue] = {
    import org.pkl.core.{PNull, PObject}

    value match {
      case null => None
      case _: PNull => None
      case s: String => Some(BufferedValue.Str(s, 0))
      case b: java.lang.Boolean => Some(if (b) BufferedValue.True(0) else BufferedValue.False(0))
      case i: java.lang.Integer => Some(BufferedValue.Int32(i, 0))
      case l: java.lang.Long => Some(BufferedValue.Int64(l, 0))
      case d: java.lang.Double => Some(BufferedValue.Float64String(d.toString, 0))
      case f: java.lang.Float => Some(BufferedValue.Float32(f, 0))
      case xs: java.util.List[?] =>
        Some(BufferedValue.Arr(
          xs.asScala.flatMap(toBufferedValue(scriptFile, _)).to(mutable.ArrayBuffer),
          0
        ))
      case map: java.util.Map[?, ?] =>
        val entries = map.asScala.iterator.flatMap {
          case (key: String, v) =>
            toBufferedValue(scriptFile, v).map(value =>
              ((BufferedValue.Str(key, 0): BufferedValue), value)
            )
          case _ => None
        }.to(mutable.ArrayBuffer)
        Some(BufferedValue.Obj(entries, jsonableKeys = true, index = 0))
      case obj: PObject =>
        val entries = obj.getProperties.asScala.iterator.flatMap {
          case ("modules", value) =>
            Some(convertModules(scriptFile, value)).toSeq.flatten.map { case (k, v) =>
              ((BufferedValue.Str(k.value, k.index): BufferedValue), v)
            }
          case (key, v) =>
            toBufferedValue(scriptFile, v).map(value => ((BufferedValue.Str(key, 0): BufferedValue), value))
        }.to(mutable.ArrayBuffer)
        Some(BufferedValue.Obj(entries, jsonableKeys = true, index = 0))
      case other => Some(BufferedValue.Str(other.toString, 0))
    }
  }
}
