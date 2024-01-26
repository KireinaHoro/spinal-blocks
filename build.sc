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
object spinalIdslPlugin extends deps.spinalhdl.build.IdslPlugin with SpinalDep { def name = "idslplugin" }

object blocks extends Cross[BlocksModule](scalaVersions)
trait BlocksModule extends SbtModule with CrossSbtModule {
  def spinalDeps = Agg(spinalCore, spinalLib)
  def spinalIdslDep = spinalIdslPlugin

  override def millSourcePath = os.pwd
  override def sources = T.sources(
    millSourcePath / "blocks"
  )
  override def resources = T.sources {
    super.resources() :+ PathRef(millSourcePath / "deps")
  }

  override def scalacOptions = super.scalacOptions() ++ spinalIdslDep.pluginOptions()
  override def moduleDeps = super.moduleDeps ++ spinalDeps
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.3",
  )
}

// vi: ft=scala
