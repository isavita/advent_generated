import scala.io.Source
import scala.collection.mutable

object DockingData {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toSeq
    val mem = mutable.Map[Long, Long]()
    var mask = ""

    for (line <- lines) {
      if (line.startsWith("mask")) {
        mask = line.split(" = ")(1)
      } else {
        val addr = line.split("\\[")(1).split("]")(0).toLong
        val value = line.split(" = ")(1).toLong
        val addrBin = addr.toBinaryString.reverse.padTo(36, '0').reverse
        var floating = ""
        var addrMasked = ""

        for (i <- 0 until 36) {
          if (mask(i) == 'X') {
            floating += 'X'
            addrMasked += addrBin(i)
          } else if (mask(i) == '1') {
            floating += '1'
            addrMasked += '1'
          } else {
            floating += '0'
            addrMasked += addrBin(i)
          }
        }

        val addrs = getAllAddresses(addrMasked, floating)
        for (addr <- addrs) {
          mem(java.lang.Long.parseLong(addr, 2)) = value
        }
      }
    }

    println(mem.values.sum)
  }

  def getAllAddresses(addr: String, floating: String): Seq[String] = {
    if (!floating.contains('X')) {
      Seq(addr)
    } else {
      val idx = floating.indexOf('X')
      val addr0 = addr.updated(idx, '0')
      val addr1 = addr.updated(idx, '1')
      val floating0 = floating.updated(idx, '0')
      val floating1 = floating.updated(idx, '1')
      getAllAddresses(addr0, floating0) ++ getAllAddresses(addr1, floating1)
    }
  }
}