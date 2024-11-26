
import scala.io.Source
import scala.collection.mutable

object MineCartMadness {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Cart(var x: Int, var y: Int, var direction: Direction, var turnState: Int = 0)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    val (tracks, carts) = parseInput(input)
    val firstCrashLocation = simulateCarts(tracks, carts)
    println(s"First crash location: $firstCrashLocation")
  }

  def parseInput(input: Array[String]): (Array[Array[Char]], mutable.ArrayBuffer[Cart]) = {
    val tracks = input.map(_.toCharArray)
    val carts = mutable.ArrayBuffer[Cart]()

    for (y <- input.indices; x <- input(y).indices) {
      input(y)(x) match {
        case '^' => 
          carts += Cart(x, y, Up)
          tracks(y)(x) = '|'
        case 'v' => 
          carts += Cart(x, y, Down)
          tracks(y)(x) = '|'
        case '<' => 
          carts += Cart(x, y, Left)
          tracks(y)(x) = '-'
        case '>' => 
          carts += Cart(x, y, Right)
          tracks(y)(x) = '-'
        case _ => // Do nothing
      }
    }

    (tracks, carts)
  }

  def simulateCarts(tracks: Array[Array[Char]], carts: mutable.ArrayBuffer[Cart]): String = {
    while (true) {
      // Sort carts by y, then x
      val sortedCarts = carts.sortBy(cart => (cart.y, cart.x))

      for (cart <- sortedCarts) {
        // Move cart
        cart.direction match {
          case Up => cart.y -= 1
          case Down => cart.y += 1
          case Left => cart.x -= 1
          case Right => cart.x += 1
        }

        // Check for collision
        if (carts.count(c => c.x == cart.x && c.y == cart.y) > 1) {
          return s"${cart.x},${cart.y}"
        }

        // Handle track changes
        tracks(cart.y)(cart.x) match {
          case '+' => handleIntersection(cart)
          case '/' => handleCurve(cart, tracks(cart.y)(cart.x))
          case '\\' => handleCurve(cart, tracks(cart.y)(cart.x))
          case _ => // Straight track, no change
        }
      }
    }
    ""
  }

  def handleIntersection(cart: Cart): Unit = {
    cart.turnState match {
      case 0 => // Turn left
        cart.direction = cart.direction match {
          case Up => Left
          case Down => Right
          case Left => Down
          case Right => Up
        }
      case 1 => // Go straight
        // No direction change
      case 2 => // Turn right
        cart.direction = cart.direction match {
          case Up => Right
          case Down => Left
          case Left => Up
          case Right => Down
        }
    }
    cart.turnState = (cart.turnState + 1) % 3
  }

  def handleCurve(cart: Cart, curve: Char): Unit = {
    cart.direction = (cart.direction, curve) match {
      case (Up, '/') => Right
      case (Up, '\\') => Left
      case (Down, '/') => Left
      case (Down, '\\') => Right
      case (Left, '/') => Down
      case (Left, '\\') => Up
      case (Right, '/') => Up
      case (Right, '\\') => Down
    }
  }
}
