package jsteward

package object blocks {
  def hexToBytesBE(bytes: String): List[Byte] = bytes.grouped(2).map(Integer.parseInt(_, 16).toByte).toList
}
