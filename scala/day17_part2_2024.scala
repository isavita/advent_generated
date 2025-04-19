import scala.io.Source
import scala.collection.mutable.{ListBuffer, HashSet, Stack} // Need mutable Stack and Set
import scala.util.matching.Regex
import scala.math.BigInt
import scala.util.control.NonFatal

object Day17Part2Optimized {

  // Structure to hold mutable register state
  case class Registers(var a: BigInt, var b: BigInt, var c: BigInt)

  // Function to get the value of a combo operand
  def getComboValue(operand: Int, regs: Registers): BigInt = {
    operand match {
      case 0 => BigInt(0)
      case 1 => BigInt(1)
      case 2 => BigInt(2)
      case 3 => BigInt(3)
      case 4 => regs.a
      case 5 => regs.b
      case 6 => regs.c
      case 7 => throw new IllegalArgumentException("Combo operand 7 is reserved")
      case _ => throw new IllegalArgumentException(s"Invalid combo operand: $operand")
    }
  }

 // Helper for correct modulo (always 0-7)
 implicit class BigIntModOps(val bi: BigInt) extends AnyVal {
    def fmod(divisor: BigInt): BigInt = {
        val result = bi % divisor
        if (result < 0) result + divisor else result
    }
 }

  // Function to simulate the computer - unchanged from previous refined version
  // Returns only the output vector, as final registers aren't needed for the check
  def simulate(initialA: BigInt, initialB: BigInt, initialC: BigInt, program: Vector[Int]): Vector[Int] = {
    val regs = Registers(initialA, initialB, initialC)
    var ip = 0
    val output = ListBuffer[Int]()
    var instructionCount = 0
    // Keep a reasonable instruction limit to detect genuine loops
    val maxInstructions = 1000000 // Reduced from 50M, simulation should be short if it matches

    while (ip >= 0 && ip < program.length && instructionCount < maxInstructions) {
      instructionCount += 1
      val opcode = program(ip)

      if (ip + 1 >= program.length) {
         // Specific check: only halt if operand is needed
         if (opcode != 4) { // Opcode 4 ignores operand
             ip = -1 // Halt
         } else {
             // Execute opcode 4 even without operand
             regs.b = regs.b ^ regs.c
             ip += 2 // Still need to advance IP past the (non-existent) operand position conceptually
         }
      } else {
        val operand = program(ip + 1)
        var nextIp = ip + 2

        try {
          opcode match {
            case 0 => // adv A = A / (2^combo)
              val comboVal = getComboValue(operand, regs)
              if (comboVal.isValidInt && comboVal >= 0) {
                val exponent = comboVal.intValue
                try {
                  val divisor = BigInt(2).pow(exponent)
                  if (divisor != 0) regs.a = regs.a / divisor else regs.a = 0
                } catch { case _: ArithmeticException => regs.a = 0 }
              } else { regs.a = 0 }

            case 1 => regs.b = regs.b ^ BigInt(operand) // bxl B = B XOR literal
            case 2 => regs.b = getComboValue(operand, regs).fmod(8) // bst B = combo % 8
            case 3 => // jnz if A != 0, ip = literal
              if (regs.a != 0) {
                 if (operand >= 0 && operand < program.length) { // Check jump target validity more strictly?
                    nextIp = operand
                 } else {
                    // Invalid jump target - treat as halt? Example doesn't cover this. Let's halt.
                    // println(s"Warning: Invalid jump target $operand at ip $ip from A=$initialA. Halting.")
                    nextIp = -1
                 }
              }
            case 4 => regs.b = regs.b ^ regs.c // bxc B = B XOR C
            case 5 => output += getComboValue(operand, regs).fmod(8).toInt // out output (combo % 8)
            case 6 => // bdv B = A / (2^combo)
              val comboVal = getComboValue(operand, regs)
               if (comboVal.isValidInt && comboVal >= 0) {
                val exponent = comboVal.intValue
                 try {
                   val divisor = BigInt(2).pow(exponent)
                   if (divisor != 0) regs.b = regs.a / divisor else regs.b = 0
                 } catch { case _: ArithmeticException => regs.b = 0 }
              } else { regs.b = 0 }

            case 7 => // cdv C = A / (2^combo)
               val comboVal = getComboValue(operand, regs)
               if (comboVal.isValidInt && comboVal >= 0) {
                val exponent = comboVal.intValue
                 try {
                   val divisor = BigInt(2).pow(exponent)
                   if (divisor != 0) regs.c = regs.a / divisor else regs.c = 0
                 } catch { case _: ArithmeticException => regs.c = 0 }
              } else { regs.c = 0 }

            case _ => // Invalid opcode
               // println(s"Error: Invalid opcode $opcode at ip $ip. Halting.")
               nextIp = -1
          } // end match opcode
          ip = nextIp
        } catch {
            case NonFatal(e) =>
            //   println(s"Runtime Error at ip $ip for A=$initialA: ${e.getMessage}. Halting.")
              ip = -1 // Force halt
        }
      } // end else (operand exists)

      // Optimization: If output already deviates from target, stop early?
      // Only useful if we were comparing full output, but Go version suggests we only need the first.
      // Let's stick to the Go logic for now.

    } // end while loop

    // Don't warn about instruction limit here, as many checks might hit it legitimately.
    // If instruction limit is hit, the output might be incomplete / wrong.
    output.toVector
  }


  // --- Reverse Check Function (inspired by Go code) ---
  // State for the backward search stack
  case class CheckState(depth: Int, score: BigInt)

  def findValidA(initialB: BigInt, initialC: BigInt, targetProgram: Vector[Int]): List[BigInt] = {
      val validScores = ListBuffer[BigInt]()
      val stack = Stack[CheckState]()
      val seen = HashSet[CheckState]() // Memoization/Cycle detection

      // Start checking from the end of the program (depth 0) with score 0
      val initialState = CheckState(0, BigInt(0))
      stack.push(initialState)
      seen.add(initialState)

      val targetLen = targetProgram.length
      var iterations = 0L
      val maxIterations = 100000000L // Safety break for the check itself

      println(s"Starting reverse check. Target length: $targetLen")

      while (stack.nonEmpty && iterations < maxIterations) {
          iterations += 1
          if (iterations % 100000 == 0) {
              println(s"Check iterations: $iterations, Stack size: ${stack.size}, Seen size: ${seen.size}")
          }

          val currentState = stack.pop()
          val depth = currentState.depth
          val score = currentState.score

          if (depth == targetLen) {
              // Reached the beginning (matched all digits backwards)
              // Now, run a final *full* simulation to be absolutely sure
              val finalOutput = simulate(score, initialB, initialC, targetProgram)
              if (finalOutput == targetProgram) {
                  println(s"  >> Found potential valid A: $score")
                  validScores += score
              } else {
                  // This shouldn't happen if the intermediate checks were correct, but good to know
                  // println(s"  >> Score $score reached end but failed final validation. Output: ${finalOutput.mkString(",")}")
              }
          } else {
              // Try extending the score with each possible digit (0-7)
              val expectedDigit = targetProgram(targetLen - 1 - depth) // Digit we expect at this depth

              for (i <- 0 to 7) {
                  val newScore = BigInt(i) + 8 * score
                  // Optimization: Maybe skip if newScore becomes excessively large?
                  // Pruning could be risky though.

                  // --- Validation Step ---
                  // Run simulation with A = newScore
                  val resultOutput = simulate(newScore, initialB, initialC, targetProgram)

                  // Check if the *first* output matches the expected digit for this depth
                  if (resultOutput.nonEmpty && resultOutput.head == expectedDigit) {
                      // If it matches, this path is potentially valid, continue exploration
                      val nextState = CheckState(depth + 1, newScore)
                      if (!seen.contains(nextState)) {
                          stack.push(nextState)
                          seen.add(nextState)
                      }
                  }
                  // If it doesn't match, prune this path (do nothing)
              }
          }
      } // end while loop

      if (iterations >= maxIterations) {
          println("Warning: Reverse check hit iteration limit.")
      }

      validScores.toList
  }


  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines = Source.fromFile(filename).getLines().toList

    val regARegex = """Register A: (\d+)""".r
    val regBRegex = """Register B: (\d+)""".r
    val regCRegex = """Register C: (\d+)""".r
    val programRegex = """Program: ([\d,]+)""".r

    var initialB: BigInt = BigInt(-1)
    var initialC: BigInt = BigInt(-1)
    var program: Vector[Int] = Vector()

    lines.foreach {
      case regARegex(_) => // Ignore initial A
      case regBRegex(b) => initialB = BigInt(b)
      case regCRegex(c) => initialC = BigInt(c)
      case programRegex(p) => program = p.split(',').map(_.trim.toIntOption).collect { case Some(i) if i >= 0 && i <= 7 => i }.toVector
      case s if s.trim.isEmpty =>
      case other => println(s"Warning: Ignoring unrecognized line: '$other'")
    }

    if (initialB < 0 || initialC < 0 || program.isEmpty || program.exists(i => i < 0 || i > 7) ) { // Removed length check, maybe not always even?
       println("Error: Invalid input format. Check B, C, program (must be 0-7 integers).")
       System.exit(1)
    }

    println(s"Parsed initial state: B=$initialB, C=$initialC")
    println(s"Program length: ${program.length}")
    // println(s"Target program output: ${program.mkString(",")}")

    val startTime = System.nanoTime()

    val validValues = findValidA(initialB, initialC, program)

    val endTime = System.nanoTime()
    val durationMs = (endTime - startTime) / 1_000_000

    println(s"\n--- Reverse Check Completed ---")
    println(s"Check took ${durationMs} ms.")

    if (validValues.isEmpty) {
        println("No valid initial A value found.")
    } else {
        println(s"Found ${validValues.length} potential valid A values.")
        // Find the minimum positive value
        val positiveValues = validValues.filter(_ > 0)
        if (positiveValues.isEmpty) {
            println("No positive valid initial A value found.")
            // Print minimum non-positive if exists?
             if (validValues.nonEmpty) println(s"Minimum overall value found: ${validValues.min}")
        } else {
            val minValue = positiveValues.min
            println(s"Lowest positive initial value for A: $minValue")
            // Final answer to be printed to console
            println(minValue)
        }
    }
  }
}
