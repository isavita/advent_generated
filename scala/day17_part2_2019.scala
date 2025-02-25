
import scala.collection.mutable
import scala.io.Source

object IntcodeComputer {
  def run(memory: mutable.Map[Long, Long], inputs: mutable.Queue[Long]): (mutable.Queue[Long], Boolean) = {
    val outputs = mutable.Queue[Long]()
    var pointer = 0L
    var relativeBase = 0L
    var halted = false

    def getParam(mode: Long, param: Long): Long = mode match {
      case 0 => memory.getOrElse(param, 0)
      case 1 => param
      case 2 => memory.getOrElse(relativeBase + param, 0)
    }

    def setParam(mode: Long, param: Long, value: Long): Unit = mode match {
      case 0 => memory(param) = value
      case 2 => memory(relativeBase + param) = value
    }

    while (!halted) {
      val instruction = memory.getOrElse(pointer, 0L)
      val opcode = instruction % 100
      val modes = Seq(
        (instruction / 100) % 10,
        (instruction / 1000) % 10,
        (instruction / 10000) % 10
      )

      opcode match {
        case 1 =>
          val (param1, param2, param3) = (memory(pointer + 1), memory(pointer + 2), memory(pointer + 3))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          setParam(modes(2), param3, val1 + val2)
          pointer += 4
        case 2 =>
          val (param1, param2, param3) = (memory(pointer + 1), memory(pointer + 2), memory(pointer + 3))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          setParam(modes(2), param3, val1 * val2)
          pointer += 4
        case 3 =>
          if (inputs.isEmpty) return (outputs, halted)
          val param1 = memory(pointer + 1)
          setParam(modes(0), param1, inputs.dequeue())
          pointer += 2
        case 4 =>
          val param1 = memory(pointer + 1)
          outputs.enqueue(getParam(modes(0), param1))
          pointer += 2
        case 5 =>
          val (param1, param2) = (memory(pointer + 1), memory(pointer + 2))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          pointer = if (val1 != 0) val2 else pointer + 3
        case 6 =>
          val (param1, param2) = (memory(pointer + 1), memory(pointer + 2))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          pointer = if (val1 == 0) val2 else pointer + 3
        case 7 =>
          val (param1, param2, param3) = (memory(pointer + 1), memory(pointer + 2), memory(pointer + 3))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          setParam(modes(2), param3, if (val1 < val2) 1 else 0)
          pointer += 4
        case 8 =>
          val (param1, param2, param3) = (memory(pointer + 1), memory(pointer + 2), memory(pointer + 3))
          val (val1, val2) = (getParam(modes(0), param1), getParam(modes(1), param2))
          setParam(modes(2), param3, if (val1 == val2) 1 else 0)
          pointer += 4
        case 9 =>
          val param1 = memory(pointer + 1)
          relativeBase += getParam(modes(0), param1)
          pointer += 2
        case 99 =>
          halted = true
        case _ =>
          throw new Exception(s"Unknown opcode: $opcode")
      }
    }
    (outputs, halted)
  }
}

object Day17 {

  def parseMap(output: mutable.Queue[Long]): List[List[Char]] = {
    val grid = mutable.ListBuffer[List[Char]]()
    val line = mutable.ListBuffer[Char]()
    output.foreach { c =>
      if (c == 10) {
        if (line.nonEmpty) {
          grid += line.toList
          line.clear()
        }
      } else {
        line += c.toChar
      }
    }
    if (line.nonEmpty) grid += line.toList
    grid.toList
  }

  def findIntersections(grid: List[List[Char]]): List[(Int, Int)] = {
    val intersections = mutable.ListBuffer[(Int, Int)]()
    for (y <- 1 until grid.length - 1; x <- 1 until grid(0).length - 1) {
      if (grid(y)(x) == '#') {
        if (grid(y - 1)(x) == '#' && grid(y + 1)(x) == '#' && grid(y)(x - 1) == '#' && grid(y)(x + 1) == '#') {
          intersections += ((x, y))
        }
      }
    }
    intersections.toList
  }

  def findRobot(grid: List[List[Char]]): Option[(Int, Int, Char)] = {
    for (y <- grid.indices; x <- grid(0).indices) {
      if ("^v<>".contains(grid(y)(x))) {
        return Some((x, y, grid(y)(x)))
      }
    }
    None
  }

  def turnLeft(direction: Char): Char = direction match {
    case '^' => '<'
    case '<' => 'v'
    case 'v' => '>'
    case '>' => '^'
  }

  def turnRight(direction: Char): Char = direction match {
    case '^' => '>'
    case '>' => 'v'
    case 'v' => '<'
    case '<' => '^'
  }

  def moveForward(x: Int, y: Int, direction: Char): (Int, Int) = direction match {
    case '^' => (x, y - 1)
    case 'v' => (x, y + 1)
    case '<' => (x - 1, y)
    case '>' => (x + 1, y)
  }

  def getMovementPath(grid: List[List[Char]], startX: Int, startY: Int, startDir: Char): List[String] = {
    var (x, y, direction) = (startX, startY, startDir)
    val path = mutable.ListBuffer[String]()
    var steps = 0

    while (true) {
      val (nextX, nextY) = moveForward(x, y, direction)
      if (nextY >= 0 && nextY < grid.length && nextX >= 0 && nextX < grid(0).length && grid(nextY)(nextX) == '#') {
        x = nextX
        y = nextY
        steps += 1
      } else {
        if (steps > 0) {
          path += steps.toString
          steps = 0
        }
        val leftDir = turnLeft(direction)
        val (nextLeftX, nextLeftY) = moveForward(x, y, leftDir)
        if (nextLeftY >= 0 && nextLeftY < grid.length && nextLeftX >= 0 && nextLeftX < grid(0).length && grid(nextLeftY)(nextLeftX) == '#') {
          path += "L"
          direction = leftDir
        } else {
          val rightDir = turnRight(direction)
          val (nextRightX, nextRightY) = moveForward(x, y, rightDir)
          if (nextRightY >= 0 && nextRightY < grid.length && nextRightX >= 0 && nextRightX < grid(0).length && grid(nextRightY)(nextRightX) == '#') {
            path += "R"
            direction = rightDir
          } else {
            return path.toList
          }
        }
      }
    }
    path.toList
  }

  def compressMovement(path: List[String]): Option[(String, String, String, String)] = {

    def isValidRoutine(routine: String): Boolean = routine.length <= 20

    def replaceSequence(seq: List[String], pattern: List[String], replacement: String): List[String] = {
      val res = mutable.ListBuffer[String]()
      var i = 0
      while (i < seq.length) {
        if (seq.slice(i, i + pattern.length) == pattern) {
          res += replacement
          i += pattern.length
        } else {
          res += seq(i)
          i += 1
        }
      }
      res.toList
    }
    val pathStr = path.mkString(",")
    val tokens = pathStr.split(",").toList

    val maxFunctionLength = 20
    val maxPatternLength = 10

        for (aLen <- 1 to maxPatternLength) {
          val aPattern = tokens.take(aLen)
          val aStr = aPattern.mkString(",")
          if (aStr.length <= maxFunctionLength) {
            val tokensAfterA = replaceSequence(tokens, aPattern, "A")

            for (bStart <- aLen until tokens.length) {
                for(bLen <- 1 to maxPatternLength) {
                  val bEnd = bStart + bLen;
                  if (bEnd <= tokens.length){

                    val bPattern = tokens.slice(bStart,bEnd)
                    val bStr = bPattern.mkString(",")

                    if(bStr.length <= maxFunctionLength) {
                      val tokensAfterB = replaceSequence(tokensAfterA, bPattern, "B")

                      for (cStart <- bEnd until tokens.length){
                        for(cLen <- 1 to maxPatternLength){
                            val cEnd = cStart + cLen;
                            if(cEnd <= tokens.length){

                                val cPattern = tokens.slice(cStart, cEnd)
                                val cStr = cPattern.mkString(",")

                                if(cStr.length <= maxFunctionLength){
                                  val tokensAfterC = replaceSequence(tokensAfterB, cPattern, "C")
                                  var mainTokens = tokensAfterC
                                  var changed = true
                                  while(changed){
                                    changed = false;
                                    val tempTokens = mainTokens
                                    mainTokens = Nil
                                    var i = 0;
                                    while(i< tempTokens.length){

                                      if(tempTokens.slice(i, i+ aPattern.length) == aPattern){
                                        mainTokens = mainTokens :+ "A"
                                        i += aPattern.length
                                        changed = true
                                      }
                                      else if (tempTokens.slice(i, i+ bPattern.length) == bPattern){
                                        mainTokens = mainTokens :+ "B"
                                        i += bPattern.length
                                        changed = true;
                                      }
                                      else if(tempTokens.slice(i, i+ cPattern.length) == cPattern) {
                                        mainTokens = mainTokens :+ "C"
                                        i+= cPattern.length
                                        changed = true;
                                      } else {
                                        mainTokens = mainTokens :+ tempTokens(i)
                                        i+=1
                                      }
                                    }
                                  }

                                  val mainRoutine = mainTokens.mkString(",")
                                  if (mainRoutine.forall("ABC,".contains(_)) && mainRoutine.length <= 20) {
                                          val functionA = aPattern.mkString(",")
                                          val functionB = bPattern.mkString(",")
                                          val functionC = cPattern.mkString(",")
                                          if (isValidRoutine(functionA) && isValidRoutine(functionB) && isValidRoutine(functionC)){
                                            return Some((mainRoutine, functionA, functionB, functionC))
                                          }
                                  }
                                }
                            }
                        }
                      }
                    }
                  }
                }
            }
          }
        }
    None
  }

  def main(args: Array[String]): Unit = {
    val program = Source.fromFile("input.txt").mkString.trim.split(",").map(_.toLong).toIndexedSeq
    val memory = mutable.Map[Long, Long]()
    program.zipWithIndex.foreach { case (value, index) => memory(index) = value }

    val (outputsPart1, _) = IntcodeComputer.run(memory.clone(), mutable.Queue[Long]())
    val grid = parseMap(outputsPart1)
    val intersections = findIntersections(grid)
    val alignmentSum = intersections.map { case (x, y) => x * y }.sum
    println(s"Part One: $alignmentSum")

    val robot = findRobot(grid).getOrElse(throw new Exception("Robot not found"))
    val (startX, startY, startDir) = robot
    val movementPath = getMovementPath(grid, startX, startY, startDir)

    val (mainRoutine, functionA, functionB, functionC) = compressMovement(movementPath)
    .getOrElse(throw new Exception("Compression Failed"))

    val inputLines = List(mainRoutine, functionA, functionB, functionC, "n")
    val movementInputs = mutable.Queue[Long]()
    inputLines.foreach(line => (line.map(_.toLong) :+ 10L).foreach(movementInputs.enqueue(_)))

    val modifiedMemory = memory.clone()
    modifiedMemory(0) = 2L
    val (outputsPart2, _) = IntcodeComputer.run(modifiedMemory, movementInputs)
    println(s"Part Two: ${outputsPart2.last}")
  }
}
