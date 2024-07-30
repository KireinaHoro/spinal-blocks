import mill._
import mill.util._
import scalalib._

import $file.deps.spinalhdl.build

val scalaVersions = Seq("2.13.12")

trait SpinalDep { this: SbtModule =>
  def crossValue = scalaVersions.head
  def name: String
  override def millSourcePath = os.pwd / "deps" / "spinalhdl" / name
}

object spinalCore extends deps.spinalhdl.build.Core with SpinalDep { def name = "core" }
object spinalLib extends deps.spinalhdl.build.Lib with SpinalDep { def name = "lib" }
object spinalTester extends deps.spinalhdl.build.CrossTester with SpinalDep { def name = "tester" }
object spinalIdslPlugin extends deps.spinalhdl.build.IdslPlugin with SpinalDep { def name = "idslplugin" }

trait SpinalPlugin extends ScalaModule {
  def pluginOptions: T[Seq[String]]
}

object blocks extends Cross[BlocksModule](scalaVersions)
trait BlocksModule extends ScalaModule with CrossScalaModule { outer =>
  def spinalDeps: Agg[ScalaModule] = Agg(spinalCore, spinalLib)
  def spinalPluginOptions: T[Seq[String]] = spinalIdslPlugin.pluginOptions

  override def millSourcePath = os.pwd
  override def sources = T.sources(
    millSourcePath / "blocks"
  )
  override def resources = T.sources {
    super.resources() :+ PathRef(millSourcePath / "deps")
  }

  override def scalacOptions = super.scalacOptions() ++ spinalPluginOptions()
  override def moduleDeps = super.moduleDeps ++ spinalDeps
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.3",
  )

  object test extends ScalaTests with TestModule.ScalaTest {
    override def moduleDeps = super.moduleDeps ++ Agg(spinalTester)
    override def millSourcePath = outer.millSourcePath
    override def sources = T.sources(millSourcePath / "tests")
  }
}

trait BlocksTester extends ScalaModule with CrossScalaModule {
  def spinalDeps: Agg[ScalaModule] = Agg(spinalCore, spinalLib, spinalTester)
  def spinalPluginOptions: T[Seq[String]] = spinalIdslPlugin.pluginOptions
  override def millSourcePath = os.pwd
  override def sources = T.sources(
    millSourcePath / "tests"
  )
  override def scalacOptions = super.scalacOptions() ++ spinalPluginOptions()
  override def moduleDeps = super.moduleDeps ++ spinalDeps ++ Agg(blocks(crossScalaVersion))
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.3",
  )
}

// vi: ft=scala
