
import scala.io.Source

object BoxCalculator {
  val hashTableSize = 256

  case class Step(label: String, numBox: Int, operation: String, number: Option[Int])

  def hashString(str: String): Int = {
    str.foldLeft(0)((res, char) => (res + char.toInt) * 17 % hashTableSize)
  }

  def parseStep(stepStr: String): Step = {
    val label = stepStr.takeWhile(c => c != '=' && c != '-').trim
    val numBox = hashString(label)
    val operation = stepStr(label.length).toString
    val number = if (operation == "=") Some(stepStr.drop(label.length + 1).toInt) else None
    Step(label, numBox, operation, number)
  }

  def getBoxes(stepsStr: Seq[String]): Map[Int, Seq[Map[String, Int]]] = {
    stepsStr.foldLeft(Map.empty[Int, Seq[Map[String, Int]]]) { (boxes, stepStr) =>
      val step = parseStep(stepStr)
      val boxContents = boxes.getOrElse(step.numBox, Seq.empty)

      step.operation match {
        case "-" =>
          val updatedContents = boxContents.flatMap { content =>
            if (content.contains(step.label)) None else Some(content)
          }
          if (updatedContents.isEmpty) boxes - step.numBox else boxes.updated(step.numBox, updatedContents)

        case "=" =>
          val updatedContents = boxContents.foldLeft(Seq.empty[Map[String, Int]]) { (acc, content) =>
            if (content.contains(step.label)) {
              acc :+ content.updated(step.label, step.number.get)
            } else {
              acc :+ content
            }
          } :+ Map(step.label -> step.number.getOrElse(0))

          boxes.updated(step.numBox, updatedContents.distinct)
      }
    }
  }

  def calculatePower(boxes: Map[Int, Seq[Map[String, Int]]]): Int = {
    (0 until hashTableSize).foldLeft(0) { (res, iBox) =>
      res + boxes.get(iBox).map { boxContents =>
        boxContents.zipWithIndex.foldLeft(0) { (innerRes, content) =>
          innerRes + content._1.values.sum * (iBox + 1) * (content._2 + 1)
        }
      }.getOrElse(0)
    }
  }

  def solve(input: Seq[String]): Int = {
    val stepsStr = input.head.split(",").map(_.trim)
    val boxes = getBoxes(stepsStr)
    calculatePower(boxes)
  }

  def readFile(fileName: String): Seq[String] = {
    Source.fromFile(fileName).getLines().toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = readFile("input.txt")
    println(solve(input))
  }
}
