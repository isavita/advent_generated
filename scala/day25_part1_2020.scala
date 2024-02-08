
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val cardPublicKey = input(0).toLong
  val doorPublicKey = input(1).toLong

  def findLoopSize(publicKey: Long): Int = {
    var value = 1L
    var loopSize = 0

    while (value != publicKey) {
      value = (value * 7) % 20201227
      loopSize += 1
    }

    loopSize
  }

  def transformSubjectNumber(subjectNumber: Long, loopSize: Int): Long = {
    var value = 1L

    for (_ <- 1 to loopSize) {
      value = (value * subjectNumber) % 20201227
    }

    value
  }

  val cardLoopSize = findLoopSize(cardPublicKey)
  val doorLoopSize = findLoopSize(doorPublicKey)

  val encryptionKey = transformSubjectNumber(doorPublicKey, cardLoopSize)

  println(encryptionKey)
}
