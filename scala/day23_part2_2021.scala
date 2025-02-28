
import scala.collection.mutable.{PriorityQueue, Set, Queue}

case class State(grid: Vector[Vector[Char]], energy: Int) extends Ordered[State]:
  def compare(that: State) = that.energy.compare(this.energy)

  def isDone(roomTargets: Map[(Int, Int), Char]): Boolean =
    roomTargets.forall((pos, ch) => grid(pos._1)(pos._2) == ch)

  def unsettled(roomTargets: Map[(Int, Int), Char]): Seq[(Int, Int)] =
    val hallway = grid(1).indices.filter(c => "ABCD".contains(grid(1)(c))).map(c => (1, c))
    val rooms = Seq(3,5,7,9).flatMap{ c =>
      var roomGood = true
      (grid.length - 2 to 2 by -1).flatMap{ r =>
        val current = grid(r)(c)
        val target = roomTargets(r,c)
        val unsettled = if current != '.' && (current != target || !roomGood) then Seq((r,c)) else Nil
        if current != target then roomGood = false
        unsettled
      }
    }
    hallway ++ rooms

  def moves(from: (Int, Int), roomTargets: Map[(Int, Int), Char]): Seq[(Int, Int)] =
    val ch = grid(from._1)(from._2)
    val queue = Queue(from)
    val visited = Set(from)
    val res = Set.empty[(Int, Int)]
    while queue.nonEmpty do
      val (r, c) = queue.dequeue()
      if (r,c) != from && !Seq((1,3),(1,5),(1,7),(1,9)).contains((r,c)) then
        roomTargets.get(r,c) match
          case None if from._1 != 1 => res += ((r,c))
          case Some(rc) if rc==ch && ((r+1 until grid.length-1).forall(x=> grid(x)(c)==ch)) => res += ((r,c))
          case _ =>
      Seq((-1,0),(1,0),(0,-1),(0,1)).foreach{case(dr,dc)=>
        val (nr,nc)=(r+dr,c+dc)
        if grid.indices.contains(nr) && grid.head.indices.contains(nc) && grid(nr)(nc)=='.' && !visited.contains((nr,nc)) then
          visited+=((nr,nc))
          queue.enqueue((nr,nc))
      }
    res.toSeq

def calcEnergy(ch: Char, from: (Int, Int), to: (Int, Int)): Int =
  val dist = (from._1 - 1) + (to._1 - 1) + (from._2 - to._2).abs
  dist * Map('A'->1,'B'->10,'C'->100,'D'->1000)(ch)

def solve(initGrid: Vector[Vector[Char]]): Int =
  val roomTargets = Map(
    (2,3)->'A',(3,3)->'A',(4,3)->'A',(5,3)->'A',
    (2,5)->'B',(3,5)->'B',(4,5)->'B',(5,5)->'B',
    (2,7)->'C',(3,7)->'C',(4,7)->'C',(5,7)->'C',
    (2,9)->'D',(3,9)->'D',(4,9)->'D',(5,9)->'D'
  )
  val expandedGrid = (initGrid.take(3) ++ Vector("  #D#C#B#A#  ","  #D#B#A#C#  ").map(_.toVector) ++ initGrid.drop(3)).map(_.toVector)
  val visited = Set[Vector[Vector[Char]]]()
  val heap = PriorityQueue(State(expandedGrid,0))
  
  while heap.nonEmpty do
    val cur = heap.dequeue()
    if !visited.add(cur.grid) then ()
    else if cur.isDone(roomTargets) then return cur.energy
    else
      cur.unsettled(roomTargets).foreach{from=>
        cur.moves(from, roomTargets).foreach{to=>
          val ch=cur.grid(from._1)(from._2)
          val nextGrid = cur.grid.updated(from._1, cur.grid(from._1).updated(from._2,'.')).updated(to._1, cur.grid(to._1).updated(to._2,ch))
          heap.enqueue(State(nextGrid,cur.energy+calcEnergy(ch,from,to)))
        }
      }
  -1

@main def main(): Unit =
  val lines = scala.io.Source.fromFile("input.txt").getLines().toVector
  println(solve(lines.map(_.toVector)))
