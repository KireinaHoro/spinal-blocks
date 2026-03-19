package jsteward.blocks

import org.apache.commons.io.output.TeeOutputStream
import org.scalatest._
import org.scalatest.funsuite.FixtureAnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._

import java.io.{FileOutputStream, PrintStream}
import java.util.zip.GZIPOutputStream
import scala.util.Random

object SimConfig {
  var simSeed: Option[Int] = None
  var setupSeed: Int = Random.nextInt
  var printSimLog: Boolean = false
}

abstract class DutSimFunSuite[T <: Component] extends FixtureAnyFunSuite with BeforeAndAfterAllConfigMap with ParallelTestExecution {
  import SimConfig._

  override def beforeAll(cm: ConfigMap): Unit = {
    cm.get("setupSeed").foreach { case s: String =>
      setupSeed = s.toInt
    }
    Random.setSeed(setupSeed)

    simSeed = cm.get("simSeed").map(_.asInstanceOf[String].toInt)
    cm.get("printSimLog").foreach { case b: String =>
      printSimLog = b.toBoolean
    }
  }

  type FixtureParam = T

  def workspace(name: String) = {
    val sc = dut.simConfig
    os.pwd / os.RelPath(sc._workspacePath) / sc._workspaceName / name
  }

  override def withFixture(test: OneArgTest) = {
    val testWorkspace = workspace(test.name)
    os.makeDir.all(testWorkspace)

    val lseed = simSeed.getOrElse(Random.nextInt)
    Random.setSeed(lseed)

    val logFilePath = testWorkspace / s"sim_transcript.log.gz"
    val logFileStream = new GZIPOutputStream(new FileOutputStream(logFilePath.toString), 64 * 1024)

    println(s"[info] simulation transcript at $logFilePath")

    val simOutStream = if (printSimLog) new TeeOutputStream(System.out, logFileStream) else logFileStream
    val printStream = new PrintStream(simOutStream) {
      // prepend simulation timestamp
      override def println(x: Object): Unit = {
        if (SimManagerContext.current != null) {
          print(s"[${SimManagerContext.current.manager.time}] ")
        }
        super.println(x)
      }
    }
    var outcome: Outcome = Succeeded

    try {
      val reproduceCmd = s"mill gen.test.testOnly ${getClass.getCanonicalName} -- -t \"${test.name}\" -DsetupSeed=$setupSeed -DsimSeed=$lseed -DprintSimLog=true\n"
      Console.withOut(printStream) {
        // write simulation log to a file
        println(s">>>>> Simulation transcript ${getClass.getCanonicalName} for test ${test.name}")
        println(s">>>>> To reproduce: $reproduceCmd")

        dut.doSim(test.name) { dut =>
          withFixture(test.toNoArgTest(dut)) match {
            case Failed(e) =>
              System.out.println(s"\n!!! Test failed, reproducer: $reproduceCmd")
              throw e
            case _ =>
          }
        }
      }
    } catch {
      case e: Throwable =>
        printStream.println()
        e.printStackTrace(printStream)
        outcome = Failed(e)
    } finally {
      printStream.flush()
      logFileStream.flush()
      logFileStream.close()

      if (printSimLog)
        println(s"[info] simulation transcript at $logFilePath")
    }

    outcome
  }

  def sleepCycles(n: Int)(implicit dut: T) = dut.clockDomain.waitActiveEdge(n)

  def randomSleep(maxCycles: Int, minCycles: Int = 0)(implicit dut: T) = {
    val toSleep = simRandom.between(minCycles, maxCycles)
    println(s"[info] sleeping $toSleep cycles")
    sleepCycles(toSleep)
  }

  def dut: SimCompiled[T]

  def check(expected: List[Byte], got: List[Byte]) = assert(expected == got,
    s"""data mismatch:
       |expected: "${expected.bytesToHex}"
       |got:      "${got.bytesToHex}"
       |""".stripMargin)
}
