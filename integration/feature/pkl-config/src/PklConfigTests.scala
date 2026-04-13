package mill.integration

import mill.testkit.UtestIntegrationTestSuite
import utest.*

object PklConfigTests extends UtestIntegrationTestSuite {
  override protected def allowSharedOutputDir: Boolean = false

  val tests: Tests = Tests {
    test - integrationTest { tester =>
      val resolveAllRes = tester.eval(("resolve", "_"))
      assert(resolveAllRes.isSuccess)
      val allTasks = resolveAllRes.out.linesIterator.map(_.trim).filter(_.nonEmpty).toSeq

      val compileTask = allTasks
        .map(_.trim)
        .find(task => task == "compile" || (task.endsWith(".compile") && !task.startsWith("selective.")))
        .getOrElse(throw new java.lang.AssertionError(allTasks.mkString("\n")))

      val compileRes = tester.eval((compileTask))
      assert(compileRes.isSuccess)

      val compileClasspathTask = allTasks
        .find(task => {
          val expected =
            if (compileTask == "compile") "compileClasspath"
            else s"${compileTask.stripSuffix(".compile")}.compileClasspath"
          task == expected
        })
        .getOrElse(throw new java.lang.AssertionError(allTasks.mkString("\n")))

      val classpathRes = tester.eval(("show", compileClasspathTask))
      assert(classpathRes.isSuccess)
      val normalized = classpathRes.out.replace("\\\\", "/")
      assert(normalized.contains("/lib/compile.dest/classes"))
    }
  }
}
