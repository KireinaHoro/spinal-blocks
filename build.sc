import mill._, util._, scalalib._
import mill.define.ModuleRef
import $file.deps.spinalhdl.build

val scalaVersions = Seq("2.13.12")

trait BlocksBaseModule extends CrossScalaModule {
  def blocksMod = ModuleRef(blocks(crossScalaVersion))
  def spinalDeps: Agg[ScalaModule] = Agg(spinalCore, spinalLib, spinalTester)
  def spinalPluginOptions: T[Seq[String]] = spinalIdslPlugin.pluginOptions
  override def moduleDeps = super.moduleDeps ++ spinalDeps
}

trait MySpinal { this: deps.spinalhdl.build.SpinalModule =>
  def crossValue = scalaVersions.head
  def name: String
  override def millSourcePath = os.pwd / "deps" / "spinalhdl" / name

  override def coreMod = ModuleRef(spinalCore)
  override def libMod = ModuleRef(spinalLib)
  override def idslpluginMod = ModuleRef(spinalIdslPlugin)
}

object spinalCore extends deps.spinalhdl.build.Core with MySpinal { def name = "core" }
object spinalLib extends deps.spinalhdl.build.Lib with MySpinal { def name = "lib" }
object spinalTester extends deps.spinalhdl.build.Tester with MySpinal { def name = "tester" }
object spinalIdslPlugin extends deps.spinalhdl.build.IdslPlugin with MySpinal { def name = "idslplugin" }

object blocks extends Cross[BlocksModule](scalaVersions)
trait BlocksModule extends BlocksBaseModule { outer =>
  override def millSourcePath = os.pwd
  override def sources = T.sources(
    millSourcePath / "blocks"
  )
  override def resources = T.sources {
    import os./

    val outResources = T.dest / "resources"
    os.makeDir(outResources)

    // copy verilog files to include in resources
    Seq("verilog-axi", "verilog-axis", "wb2axip")
      .flatMap(dn => os.walk(millSourcePath / "deps" / dn / "rtl"))
      .filter(_.ext == "v")
      .map(os.copy.matching { case p/dn/"rtl"/f => outResources/dn/"rtl"/f })

    super.resources() :+ PathRef(outResources)
  }

  override def scalacOptions = super.scalacOptions() ++ spinalPluginOptions()
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.3",
  )

  object test extends ScalaTests with TestModule.ScalaTest {
    override def moduleDeps = outer.moduleDeps
    override def millSourcePath = outer.millSourcePath
    override def sources = T.sources(millSourcePath / "tests")
  }
}

trait BlocksTester extends BlocksBaseModule {
  override def millSourcePath = os.pwd
  override def sources = T.sources(
    millSourcePath / "tests"
  )
  override def scalacOptions = super.scalacOptions() ++ spinalPluginOptions()
  override def moduleDeps = super.moduleDeps ++ Agg(blocksMod())
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.3",
  )
}

// vi: ft=scala
