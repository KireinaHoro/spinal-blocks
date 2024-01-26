package blackbox.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4StreamConfig

import scala.reflect.{ClassTag, classTag}

/**
 * Axi4-Stream for custom data bundles configuration
 *
 * @param payloadType Type of data bundle
 * @param idWidth     Width of the ID field in bits
 * @param destWidth   Width of the Destination field in bits
 * @param userWidth   Width of the User field in bits
 * @param useLast     Use last bit
 * @param useId       Use ID field, must specify idWidth
 * @param useDest     Use Destination field, must specify destWidth
 * @param useUser     Use User field, must specify userWidth
 */
case class Axi4StreamCustomConfig[T <: Data](payloadType: HardType[T],
                                             idWidth: Int = -1,
                                             destWidth: Int = -1,
                                             userWidth: Int = -1,
                                             useLast: Boolean = false,
                                             useId: Boolean = false,
                                             useDest: Boolean = false,
                                             useUser: Boolean = false)

object Axi4StreamCustomConfig {
  def fromStreamConfig[T <: Data](payloadType: HardType[T], config: Axi4StreamConfig) = {
    Axi4StreamCustomConfig(
      payloadType,
      config.idWidth,
      config.destWidth,
      config.userWidth,
      config.useLast,
      config.useId,
      config.useDest,
      config.useUser,
    )
  }
}

object Axi4StreamCustom {

  case class Axi4StreamCustomBundle[T <: Data](val config: Axi4StreamCustomConfig[T]) extends Bundle {
    val payload = config.payloadType()
    val id = (config.useId) generate UInt(config.idWidth bit)
    val last = (config.useLast) generate Bool()
    val dest = (config.useDest) generate UInt(config.destWidth bit)
    val user = (config.useUser) generate Bits(config.userWidth bit)

    def isLast = if (this.last != null) this.last else False

    override def clone: Axi4StreamCustomBundle[T] = Axi4StreamCustomBundle(config)

    override def bundleAssign(that: Bundle)(f: (Data, Data) => Unit): Unit = {
      that match {
        case that: Axi4StreamCustomBundle[T] => {
          assert(that.config.payloadType == this.config.payloadType, s"Axi4StreamCustom $that has different payload type (${that.config.payloadType}) than $this (${this.config.payloadType})")
          if (that.config.useId)
            assert(that.config.idWidth <= this.config.idWidth, s"Axi4StreamCustom $that directly drives stream $this with smaller ID width! (${that.config.idWidth} > ${this.config.idWidth})")
          if (that.config.useDest)
            assert(that.config.destWidth <= this.config.destWidth, s"Axi4StreamCustom $that directly drives stream $this with smaller destination width! (${that.config.destWidth} > ${this.config.destWidth})")
          if (that.config.useUser)
            assert(that.config.userWidth <= this.config.userWidth, s"Axi4StreamCustom $that directly drives stream $this with smaller user width! (${that.config.userWidth} > ${this.config.userWidth})")

          this.payload := that.payload.resized
          Axi4StreamCustomBundlePriv.driveWeak(that, this, that.id, this.id, () => U(this.id.bitsRange -> false), allowResize = true, allowDrop = false)
          Axi4StreamCustomBundlePriv.driveWeak(that, this, that.last, this.last, () => False, allowResize = false, allowDrop = false)
          Axi4StreamCustomBundlePriv.driveWeak(that, this, that.dest, this.dest, () => B(this.dest.bitsRange -> true), allowResize = true, allowDrop = false)

          (this.user != null, that.user != null) match {
            case (false, false) =>
            case (true, false) => B(this.user.bitsRange -> false)
            case (false, true) => LocatedPendingError(s"${that.user} can't drive $this because the corresponding sink signal does not exist")
            case (true, true) => this.user := that.user.resized
          }
        }
      }
    }
  }

  private object Axi4StreamCustomBundlePriv {
    def driveWeak[T <: Data](source: Bundle, sink: Bundle, by: T, to: T, defaultValue: () => T, allowResize: Boolean, allowDrop: Boolean): Unit = {
      (to != null, by != null) match {
        case (false, false) =>
        case (true, false) => if (defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because the corresponding source signal does not exist")
        case (false, true) => if (!allowDrop) LocatedPendingError(s"$by can't drive $sink because the corresponding sink signal does not exist")
        case (true, true) => to := (if (allowResize) by.resized else by)
      }
    }
  }

  type Axi4StreamCustom[T <: Data] = Stream[Axi4StreamCustomBundle[T]]

  def apply[T <: Data](config: Axi4StreamCustomConfig[T]): Stream[Axi4StreamCustomBundle[T]] = Stream(Axi4StreamCustomBundle(config))

  // reflection: https://stackoverflow.com/a/21181344
  def apply[T <: Data : ClassTag](source: Stream[T]): Axi4StreamCustom[T] = {
    source.payload match {
      case data if classTag[T].runtimeClass.isInstance(data) =>
        val axisStream = Axi4StreamCustom(Axi4StreamCustomConfig(payloadType = data))
        axisStream.payload.payload := data
        axisStream.arbitrationFrom(source)
        axisStream
    }
  }

  implicit class Axi4StreamCustomRich[T <: Data](stream: Stream[Axi4StreamCustomBundle[T]]) {

    def lastFire: Bool = stream.isLast && stream.fire

    /**
     * Converts the Axi4StreamCustom into a Stream of the payload type.
     * Does not support TSTRB or TKEEP. Will only convert continuous and aligned streams.
     * If TLAST was present it will be dropped. Consider toStreamFragment
     *
     * @return Stream of payload
     */
    def toDataStream(): Stream[T] = {
      val that = Stream(stream.config.payloadType)
      if (stream.config.useLast)
        SpinalWarning(s"Axi4Stream $this converted to Stream of ${that.payloadType} discards TLAST. Consider using toFragmentStream instead.")

      that.arbitrationFrom(stream)
      that.payload := stream.payload.payload

      that
    }

    /**
     * Results a Flow of TID. To be used with toStream functions.
     *
     * @return Flow of Axi4Stream TID field
     */
    def getIdFlow(): Flow[UInt] = {
      val idFlow = Flow(if (stream.id != null) stream.id else UInt(0 bit))
      idFlow.valid := stream.valid
      idFlow.payload := stream.id
      idFlow
    }

    /**
     * Results a Flow of TDEST. To be used with toStream functions.
     *
     * @return Flow of Axi4Stream TDEST field
     */
    def getDestFlow(): Flow[UInt] = {
      val destFlow = Flow(if (stream.dest != null) stream.dest else UInt(0 bit))
      destFlow.valid := stream.valid
      destFlow.payload := stream.dest
      destFlow
    }

    /**
     * Results a Flow of TUSER. To be used with toStream functions.
     *
     * @return Flow of Axi4Stream TUSER field
     */
    def getUserFlow(): Flow[Bits] = {
      val userFlow = Flow(if (stream.user != null) stream.user else Bits(0 bit))
      userFlow.valid := stream.valid
      userFlow.payload := stream.user
      userFlow
    }
  }
}
