package mill.internal.pkl

import mill.api.Result
import mill.api.internal.{Appendable, HeaderData, Located, OneOrMore}
import upickle.core.BufferedValue

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using

object PklParser {
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
    convertModule(scriptFile, module.getClassInfo, module.getProperties.asScala.toMap)

  private def convertModule(
      scriptFile: os.Path,
      classInfo: org.pkl.core.PClassInfo[?],
      props: Map[String, Any]
  ): HeaderData = {
    val extendsValue =
      (moduleExtendsName(classInfo).toSeq ++ readTraitExtends(scriptFile, props.get("traits")))
        .map(Located(scriptFile, 0, _))

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

  private def moduleExtendsName(classInfo: org.pkl.core.PClassInfo[?]): Option[String] = {
    val schemaSegments = schemaPathSegments(classInfo)
    val effectiveSegments =
      if (
        !classInfo.isModuleClass() &&
        schemaSegments.nonEmpty &&
        schemaSegments.last != classInfo.getSimpleName
      )
        schemaSegments :+ classInfo.getSimpleName
      else schemaSegments

    effectiveSegments match {
      case Seq("api", "Module") if classInfo.isModuleClass() => Some("Module")
      case Seq("javalib", "JavaModule") if classInfo.isModuleClass() => Some("mill.javalib.JavaModule")
      case Seq("javalib", "JavaModule", "JavaTests") => Some("mill.javalib.JavaModule.JavaTests")
      case Seq("scalalib", "ScalaModule") if classInfo.isModuleClass() => Some("mill.scalalib.ScalaModule")
      case Seq("scalalib", "ScalaModule", "ScalaTests") => Some("mill.scalalib.ScalaModule.ScalaTests")
      case Seq("kotlinlib", "KotlinModule") if classInfo.isModuleClass() => Some("mill.kotlinlib.KotlinModule")
      case Seq("kotlinlib", "KotlinModule", "KotlinTests") => Some("mill.kotlinlib.KotlinModule.KotlinTests")
      case _ => None
    }
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
    appendTraitProperties(scriptFile, props.get("traits"), builder)
    props.iterator.foreach {
      case (name, _) if depFieldNames(name) => ()
      case ("modules", value) =>
        builder ++= convertModules(scriptFile, value)
      case ("traits", _) => ()
      case (name, value) =>
        toBufferedValue(scriptFile, value).foreach { buffered =>
          builder += Located(scriptFile, 0, rootKeyMapping.getOrElse(name, name)) -> buffered
        }
    }
    builder.result()
  }

  private def readTraitExtends(scriptFile: os.Path, valueOpt: Option[Any]): Seq[String] =
    valueOpt match {
      case Some(xs: java.util.List[?]) =>
        xs.asScala.collect { case obj: org.pkl.core.PObject => traitExtendsName(scriptFile, obj) }.toSeq
      case Some(xs: Seq[?]) =>
        xs.collect { case obj: org.pkl.core.PObject => traitExtendsName(scriptFile, obj) }
      case _ => Nil
    }

  private def traitExtendsName(scriptFile: os.Path, obj: org.pkl.core.PObject): String = {
    val classInfo = obj.getClassInfo
    traitExtendsName(classInfo).getOrElse(
      throw new mill.api.daemon.Result.Exception(
        s"Unsupported Pkl trait `${classInfo.getQualifiedName}` in ${scriptFile.last}"
      )
    )
  }

  private def traitExtendsName(classInfo: org.pkl.core.PClassInfo[?]): Option[String] = {
    val schemaPath = schemaPathSegments(classInfo)
    val classSuffix =
      if (classInfo.isModuleClass()) Nil
      else Seq(classInfo.getSimpleName)

    schemaPath match {
      case Seq("api", _*) => None
      case head +: rest => Some((Seq("mill", head) ++ rest ++ classSuffix).mkString("."))
      case _ => None
    }
  }

  private def schemaPathSegments(classInfo: org.pkl.core.PClassInfo[?]): Seq[String] = {
    val uri = classInfo.getModuleUri
    if (uri == null || uri.getScheme != "file") Seq.empty
    else {
      val path = os.Path(java.nio.file.Path.of(uri), mill.api.BuildCtx.workspaceRoot)
      val segments = path.segments.toSeq
      segments.lastOption match {
        case Some(fileName) if fileName.endsWith(".pkl") =>
          val withoutExt = fileName.stripSuffix(".pkl")
          segments.indexOf("schema") match {
            case -1 => Seq.empty
            case idx => segments.slice(idx + 1, segments.length - 1) :+ withoutExt
          }
        case _ => Seq.empty
      }
    }
  }

  private def appendTraitProperties(
      scriptFile: os.Path,
      valueOpt: Option[Any],
      builder: scala.collection.mutable.Builder[(Located[String], BufferedValue), Map[Located[String], BufferedValue]]
  ): Unit = {
    val traitObjects = valueOpt match {
      case Some(xs: java.util.List[?]) => xs.asScala.collect { case obj: org.pkl.core.PObject => obj }.toSeq
      case Some(xs: Seq[?]) => xs.collect { case obj: org.pkl.core.PObject => obj }
      case _ => Nil
    }
    traitObjects.foreach { obj =>
      obj.getProperties.asScala.iterator.foreach {
        case ("modules", value) =>
          builder ++= convertModules(scriptFile, value)
        case (name, value) =>
          toBufferedValue(scriptFile, value).foreach { buffered =>
            builder += Located(scriptFile, 0, rootKeyMapping.getOrElse(name, name)) -> buffered
          }
      }
    }
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
            convertModule(scriptFile, obj.getClassInfo, obj.getProperties.asScala.toMap)
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
