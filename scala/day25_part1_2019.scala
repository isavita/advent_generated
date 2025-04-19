import scala.io.Source
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.control.Breaks._
import scala.util.Try
import java.time.LocalTime // For timestamps

// --- Game Logic Enums/Classes (same as before) ---
case class Room(
    name: String,
    var connections: mutable.Map[String, Option[Room]] = mutable.Map.empty
) {
  override def equals(other: Any): Boolean = other match {
    case that: Room => this.name == that.name
    case _          => false
  }
  override def hashCode(): Int = name.hashCode
}

object Mode extends Enumeration {
  type Mode = Value
  val EXPLORE, NAVIGATE, TEST = Value
}

sealed trait EmulatorStatus
case object HALTED extends EmulatorStatus
case object OUTPUT extends EmulatorStatus
case object WAITING_FOR_INPUT extends EmulatorStatus

// --- Intcode Emulator (Python List Style using ArrayBuffer - unchanged) ---
class IntcodeEmulator(initialProgram: Seq[Long]) {
  private val memory: mutable.ArrayBuffer[Long] = mutable.ArrayBuffer.from(initialProgram)
  private val inputQueue: mutable.Queue[Long] = mutable.Queue.empty
  private var ip: Int = 0
  private var relativeBase: Long = 0L

  def addInput(value: Long): Unit = { inputQueue.enqueue(value) }
  def addStringInput(s: String): Unit = { s.foreach(char => addInput(char.toLong)) }

  private def ensureCapacity(index: Int): Unit = { if (index >= memory.length) { memory.addAll(Seq.fill(index - memory.length + 1)(0L)) } }
  private def getMem(address: Int): Long = { ensureCapacity(address); memory(address) }
  private def setMem(address: Int, value: Long): Unit = { ensureCapacity(address); memory(address) = value }

  private def getParameter(offset: Int, instruction: Long): Long = {
    val mode = (instruction / math.pow(10, offset + 1).toLong) % 10; val paramAddr = ip + offset; val param = getMem(paramAddr)
    mode.toInt match { case 0 => getMem(param.toInt); case 1 => param; case 2 => getMem((relativeBase + param).toInt); case _ => throw new RuntimeException(s"Invalid read mode: $mode") }
  }
   private def getWriteAddress(offset: Int, instruction: Long): Int = {
     val mode = (instruction / math.pow(10, offset + 1).toLong) % 10; val paramAddr = ip + offset; val param = getMem(paramAddr)
     val address = mode.toInt match { case 0 => param.toInt; case 2 => (relativeBase + param).toInt; case _ => throw new RuntimeException(s"Invalid write mode: $mode") }
     ensureCapacity(address); address
 }

  def run(): (Option[Long], EmulatorStatus) = {
    while (true) {
      val instruction = getMem(ip); val opcode = (instruction % 100).toInt
      opcode match {
        case 1 => val v1=getParameter(1,instruction); val v2=getParameter(2,instruction); val d=getWriteAddress(3,instruction); setMem(d,v1+v2); ip+=4
        case 2 => val v1=getParameter(1,instruction); val v2=getParameter(2,instruction); val d=getWriteAddress(3,instruction); setMem(d,v1*v2); ip+=4
        case 3 => if(inputQueue.isEmpty) return(None,WAITING_FOR_INPUT) else {val d=getWriteAddress(1,instruction); setMem(d,inputQueue.dequeue()); ip+=2}
        case 4 => val o=getParameter(1,instruction); ip+=2; return(Some(o),OUTPUT)
        case 5 => val c=getParameter(1,instruction); val j=getParameter(2,instruction); ip=if(c!=0)j.toInt else ip+3
        case 6 => val c=getParameter(1,instruction); val j=getParameter(2,instruction); ip=if(c==0)j.toInt else ip+3
        case 7 => val v1=getParameter(1,instruction); val v2=getParameter(2,instruction); val d=getWriteAddress(3,instruction); setMem(d,if(v1<v2)1L else 0L); ip+=4
        case 8 => val v1=getParameter(1,instruction); val v2=getParameter(2,instruction); val d=getWriteAddress(3,instruction); setMem(d,if(v1==v2)1L else 0L); ip+=4
        case 9 => val o=getParameter(1,instruction); relativeBase+=o; ip+=2
        case 99 => return(None,HALTED)
        case _ => throw new RuntimeException(s"Invalid opcode: $opcode at ip $ip (instruction: $instruction)")
      }
    }
    (None, HALTED)
  }
}


object Day25Part1_2019_PythonStyle_Fix1 {

  // --- Constants and Regex (same) ---
  val opposite: Map[String, String] = Map("north" -> "south", "south" -> "north", "west" -> "east", "east" -> "west")
  val blacklist: Set[String] = Set("photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet")
  val roomNameRegex: Regex = """^== (.+) ==$""".r
  val listItemRegex: Regex = """^- (.+)$""".r
  val takenRegex: Regex = """^You take the (.+)\.$""".r
  val droppedRegex: Regex = """^You drop the (.+)\.$""".r
  val alertPrefix = "A loud, robotic voice says \"Alert!"
  val resultRegex: Regex = """typing (\d+) on the keypad""".r

  // --- Pathfinding (same) ---
   def findPath(from: Room, to: Room): Option[List[Room]] = {
    val queue = mutable.Queue[(Room, List[Room])]()
    val visited = mutable.Set[Room](from)
    queue.enqueue((from, List(from)))
    while (queue.nonEmpty) {
      val (current, path) = queue.dequeue()
      if (current == to) return Some(path)
      current.connections.values.flatten.foreach { nextRoom => if (visited.add(nextRoom)) { queue.enqueue((nextRoom, path :+ nextRoom)) } }
    }
    None
  }

  // --- Main Logic with Fix ---
  def main(args: Array[String]): Unit = {
    val programText = Source.fromFile("input.txt").mkString.trim
    val program = programText.split(',').map(s => s.trim.toLong).toSeq
    val emulator = new IntcodeEmulator(program)

    def sendCommand(cmd: String): Unit = {
      // println(s"[${LocalTime.now}] Sending command: ${cmd.trim}") // Log command sending
      emulator.addStringInput(cmd)
    }

    // Game State
    val world = mutable.Map[String, Room]()
    val inventory = mutable.Map[String, Boolean]().withDefaultValue(false)
    var mode: Mode.Value = Mode.EXPLORE
    var path = mutable.ListBuffer[Room]()
    var checkpoint: Option[Room] = None
    var floor: Option[Room] = None
    var testDir: Option[String] = None
    var availableItems = List[String]()
    var itemMask: Int = 0
    var lastRoom: Option[Room] = None // Previous turn's room
    var lastItemsInRoom = List[String]()
    var lastDirectionTaken: Option[String] = None // Previous turn's move command
    val outputBuilder = mutable.ListBuffer[Int]()
    var turnCounter = 0L
    var alertJustHappened = false // Flag to manage state after alert

    println(s"[${LocalTime.now}] Starting simulation...")

    breakable {
      while (true) {
        turnCounter += 1
        if (turnCounter % 10000 == 0) { // Log progress less frequently now
            println(s"[${LocalTime.now}] Turn: $turnCounter, Mode: $mode, Room: ${lastRoom.map(_.name)}, PathLen: ${path.length}, Inv: ${inventory.filter(_._2).keys.size}")
        }

        val (optOutputVal, status) = emulator.run()

        status match {
          case HALTED =>
            val output = outputBuilder.map(_.toChar).mkString
            println(s"\n[${LocalTime.now}] --- HALTED ---")
            // println(output)
            resultRegex.findFirstMatchIn(output) match {
                 case Some(m) => println(s"Result Code: ${m.group(1)}")
                 case None => println("Halted, but could not find result code in final output.")
             }
            break()

          case OUTPUT =>
            optOutputVal.foreach(v => outputBuilder += v.toInt)

          case WAITING_FOR_INPUT =>
            val output = outputBuilder.map(_.toChar).mkString
            outputBuilder.clear()
            // println(s"\n[${LocalTime.now}] --- WAITING FOR INPUT (Turn $turnCounter) ---")
            // print(output)

            var currentRoomOpt: Option[Room] = None // Room identified THIS turn
            var itemsInCurrentRoom = mutable.ListBuffer[String]()
            alertJustHappened = false // Reset alert flag for this turn

            // --- Parse Output ---
            val lines = output.split('\n').map(_.trim).filter(_.nonEmpty)
            var i = 0
            while(i < lines.length) {
                val line = lines(i)
                if (line.startsWith(alertPrefix)) {
                    alertJustHappened = true // Set flag
                    // println(s"[${LocalTime.now}] !!! Alert Detected !!!")
                    if (mode == Mode.EXPLORE && lastRoom.isDefined && currentRoomOpt.isDefined && lastDirectionTaken.isDefined) {
                        // println(s"[${LocalTime.now}] Setting checkpoint...")
                        checkpoint = lastRoom; floor = currentRoomOpt; testDir = lastDirectionTaken
                        checkpoint.foreach(_.connections.update(testDir.get, floor))
                        floor.foreach(f => opposite.get(testDir.get).foreach(oppDir => f.connections.update(oppDir, checkpoint)))
                        if (path.nonEmpty && path.lastOption == lastRoom) { path.remove(path.length - 1) }
                        // *** Crucial: Clear lastRoom state because we were ejected ***
                        lastRoom = None
                        lastDirectionTaken = None
                    } else {
                         // Alert during test/nav - we were ejected, clear state
                         // println(s"[${LocalTime.now}] Alert detected (not setting checkpoint).")
                         lastRoom = None
                         lastDirectionTaken = None
                    }
                } else { // Normal parsing...
                    line match {
                        case roomNameRegex(name) => val r=world.getOrElseUpdate(name,Room(name)); currentRoomOpt=Some(r); itemsInCurrentRoom.clear(); i+=1; while(i<lines.length && lines(i).nonEmpty && !lines(i).startsWith("==") && lines(i)!="Doors here lead:" && lines(i)!="Items here:" && lines(i)!="Command?"){i+=1}; i-=1
                        case "Doors here lead:" => if(currentRoomOpt.isDefined){val c=currentRoomOpt.get; i+=1; while(i<lines.length && lines(i).startsWith("- ")){listItemRegex.findFirstMatchIn(lines(i)).foreach{m=>val d=m.group(1); c.connections.getOrElseUpdate(d,None)}; i+=1}; i-=1} else {i+=1}
                        case "Items here:" => i+=1; while(i<lines.length && lines(i).startsWith("- ")){listItemRegex.findFirstMatchIn(lines(i)).foreach(m=>itemsInCurrentRoom+=m.group(1)); i+=1}; i-=1
                        case takenRegex(item) => inventory(item)=true; currentRoomOpt=lastRoom; itemsInCurrentRoom=lastItemsInRoom.filterNot(_==item).to(mutable.ListBuffer)
                        case droppedRegex(item) => inventory(item)=false; currentRoomOpt=lastRoom; itemsInCurrentRoom=(lastItemsInRoom:+item).to(mutable.ListBuffer)
                        case "Command?" =>
                        case s if s.startsWith("You take the") =>
                        case s if s.startsWith("You drop the") =>
                        case _ =>
                    }
                }
                i += 1
            } // End parsing loop

            // --- Update Connections ---
            (lastRoom, currentRoomOpt, lastDirectionTaken) match {
                // Link only if we moved from a known room to a newly identified room via a known direction
                // AND the connection wasn't already established (e.g., by an alert)
                case (Some(last), Some(current), Some(dir)) if last.connections.get(dir).flatten.isEmpty =>
                    // println(s"[${LocalTime.now}] Linking ${last.name} --$dir--> ${current.name}")
                    last.connections.update(dir, Some(current))
                    opposite.get(dir).foreach { oppDir => current.connections.getOrElseUpdate(oppDir, None); current.connections.update(oppDir, Some(last)) }
                 case _ =>
            }

            // --- Update Last Known State (FIXED LOGIC) ---
            val previousLastRoom = lastRoom // Store the room from the *start* of this turn
            if (currentRoomOpt.isDefined) {
                // If we successfully parsed a room this turn, it becomes the new lastRoom
                lastRoom = currentRoomOpt
            } else if (alertJustHappened) {
                // If an alert happened and we didn't parse a room, we were ejected.
                // We already cleared lastRoom in the alert handler. Keep it None.
            } else {
                // If no room parsed and no alert, assume we are still in the previous room.
                // lastRoom remains unchanged (keeps value from 'previousLastRoom')
                lastRoom = previousLastRoom
            }
            lastItemsInRoom = itemsInCurrentRoom.toList
            val moveCommandSent = lastDirectionTaken // Store if a move was *intended* last turn
            lastDirectionTaken = None // Reset until a move command is sent *this* turn

            // --- Mode Logic ---
            // Use the most recently known room for decision making
            val effectiveCurrentRoom = lastRoom

            effectiveCurrentRoom match {
              case None =>
                 // This should now only happen if we start lost or an alert ejected us
                 // and the subsequent output didn't contain a room name yet.
                 println(s"[${LocalTime.now}] Warning: No known current room. Mode: $mode. Waiting.")

              case Some(current) =>
                // println(s"[${LocalTime.now}] Effective room: ${current.name}. Mode: $mode")
                mode match {
                  case Mode.EXPLORE =>
                    var actionTaken = false
                    itemsInCurrentRoom.find(item => !blacklist.contains(item) && !inventory(item)) match {
                        case Some(itemToTake) => sendCommand(s"take $itemToTake\n"); actionTaken = true
                        case None =>
                            current.connections.find { case (_, optRoom) => optRoom.isEmpty } match {
                                case Some((dir, _)) => path += current; lastDirectionTaken = Some(dir); sendCommand(s"$dir\n"); actionTaken = true
                                case None =>
                                    if (path.nonEmpty) {
                                        val backtrackTo = path.remove(path.length - 1)
                                        current.connections.find { case (_, optRoom) => optRoom.contains(backtrackTo) } match {
                                            case Some((dir, _)) => lastDirectionTaken = Some(dir); sendCommand(s"$dir\n"); actionTaken = true
                                            case None => throw new RuntimeException(s"Cannot backtrack from ${current.name} to ${backtrackTo.name}")
                                        }
                                    }
                            }
                    }
                    if (!actionTaken && path.isEmpty) { // Transition check
                        checkpoint match {
                            case None => println(s"[${LocalTime.now}] Error: Exploration finished but no checkpoint found!"); break()
                            case Some(cp) =>
                                if (current == cp) {
                                    // println(s"[${LocalTime.now}] Reached checkpoint ${cp.name}. Switching to TEST mode.")
                                    availableItems = inventory.filter(_._2).keys.toList.sorted; itemMask = 0; mode = Mode.TEST
                                } else {
                                    // println(s"[${LocalTime.now}] Exploration complete. Finding path from ${current.name} to checkpoint ${cp.name}.")
                                    findPath(current, cp) match {
                                        case Some(navPathList) => path = mutable.ListBuffer.from(navPathList.drop(1)); mode = Mode.NAVIGATE // println(s"[${LocalTime.now}] Switching to NAVIGATE mode. Path length: ${path.length}")
                                        case None => println(s"[${LocalTime.now}] Error: Cannot find path from ${current.name} to checkpoint ${cp.name}!"); break()
                                    }
                                }
                        }
                    }

                  case Mode.NAVIGATE =>
                    if (path.nonEmpty) {
                      val nextRoom = path.head // Peek, don't remove yet
                      current.connections.find { case (_, optRoom) => optRoom.contains(nextRoom) } match {
                        case Some((dir, _)) =>
                          path.remove(0) // Remove *after* finding direction
                          lastDirectionTaken = Some(dir); sendCommand(s"$dir\n")
                        case None => throw new RuntimeException(s"Navigation failed from ${current.name} to ${nextRoom.name}")
                      }
                    } else { // Reached destination
                      // println(s"[${LocalTime.now}] Navigation complete. Reached ${current.name}. Switching to TEST mode.")
                      availableItems = inventory.filter(_._2).keys.toList.sorted; itemMask = 0; mode = Mode.TEST
                    }

                  case Mode.TEST =>
                    var setupActionTaken = false
                    breakable {
                      availableItems.zipWithIndex.foreach { case (item, index) =>
                        val shouldHave = (itemMask & (1 << index)) != 0; val has = inventory(item)
                        if (shouldHave != has) { val action = if (shouldHave) "take" else "drop"; sendCommand(s"$action $item\n"); setupActionTaken = true; break() }
                      }
                    }
                    if (!setupActionTaken) {
                        testDir match {
                           case Some(dir) => lastDirectionTaken = Some(dir); sendCommand(s"$dir\n"); itemMask += 1
                           case None => println(s"[${LocalTime.now}] Error: In Test mode but testDir is unknown!"); break()
                        }
                    }
                } // end match mode
            } // end match effectiveCurrentRoom
        } // end match emulator status
      } // end while(true)
    } // end breakable
    println(s"[${LocalTime.now}] Simulation ended.")
  } // end main
} // end object
