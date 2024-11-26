
import scala.io.Source
import scala.collection.mutable

object MineCartMadness {
  // Represents a cart's direction and turning state
  case class Cart(
    var x: Int, 
    var y: Int, 
    var direction: Char, 
    var turnState: Int = 0
  )

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toArray
    
    // Part 1: Find first crash location
    val (grid, carts1) = parseInput(input)
    val firstCrashLocation = findFirstCrash(grid, carts1)
    println(s"First crash location: ${firstCrashLocation._1},${firstCrashLocation._2}")

    // Part 2: Find last remaining cart location
    val (_, carts2) = parseInput(input)
    val lastCartLocation = findLastCart(grid, carts2)
    println(s"Last cart location: ${lastCartLocation._1},${lastCartLocation._2}")
  }

  def parseInput(input: Array[String]): (Array[Array[Char]], mutable.ArrayBuffer[Cart]) = {
    val grid = input.map(_.toCharArray)
    val carts = mutable.ArrayBuffer[Cart]()

    for (y <- input.indices; x <- input(y).indices) {
      input(y)(x) match {
        case '^' => 
          carts += Cart(x, y, '^')
          grid(y)(x) = '|'
        case 'v' => 
          carts += Cart(x, y, 'v')
          grid(y)(x) = '|'
        case '<' => 
          carts += Cart(x, y, '<')
          grid(y)(x) = '-'
        case '>' => 
          carts += Cart(x, y, '>')
          grid(y)(x) = '-'
        case _ => 
      }
    }

    (grid, carts)
  }

  def findFirstCrash(grid: Array[Array[Char]], carts: mutable.ArrayBuffer[Cart]): (Int, Int) = {
    while (true) {
      // Sort carts by y, then x
      val sortedCarts = carts.sortBy(c => (c.y, c.x))

      for (cart <- sortedCarts if carts.contains(cart)) {
        moveCart(cart, grid, carts)
        
        // Check for collisions
        val collisions = carts.groupBy(c => (c.x, c.y)).filter(_._2.size > 1)
        if (collisions.nonEmpty) {
          val (x, y) = collisions.head._1
          return (x, y)
        }
      }
    }
    (-1, -1) // Unreachable
  }

  def findLastCart(grid: Array[Array[Char]], carts: mutable.ArrayBuffer[Cart]): (Int, Int) = {
    while (carts.size > 1) {
      val sortedCarts = carts.sortBy(c => (c.y, c.x))

      for (cart <- sortedCarts if carts.contains(cart)) {
        moveCart(cart, grid, carts)
        
        // Remove colliding carts
        val collisions = carts.groupBy(c => (c.x, c.y)).filter(_._2.size > 1)
        if (collisions.nonEmpty) {
          val collidingPositions = collisions.keys
          carts --= carts.filter(c => collidingPositions.exists(p => c.x == p._1 && c.y == p._2))
        }
      }
    }

    val lastCart = carts.head
    (lastCart.x, lastCart.y)
  }

  def moveCart(cart: Cart, grid: Array[Array[Char]], carts: mutable.ArrayBuffer[Cart]): Unit = {
    // Move cart based on current direction
    cart.direction match {
      case '^' => cart.y -= 1
      case 'v' => cart.y += 1
      case '<' => cart.x -= 1
      case '>' => cart.x += 1
    }

    // Update direction based on track
    grid(cart.y)(cart.x) match {
      case '/' => 
        cart.direction = cart.direction match {
          case '^' => '>'
          case 'v' => '<'
          case '<' => 'v'
          case '>' => '^'
        }
      case '\\' => 
        cart.direction = cart.direction match {
          case '^' => '<'
          case 'v' => '>'
          case '<' => '^'
          case '>' => 'v'
        }
      case '+' => 
        cart.direction = cart.direction match {
          case '^' => 
            cart.turnState match {
              case 0 => '<'
              case 1 => '^'
              case 2 => '>'
            }
          case 'v' => 
            cart.turnState match {
              case 0 => '>'
              case 1 => 'v'
              case 2 => '<'
            }
          case '<' => 
            cart.turnState match {
              case 0 => 'v'
              case 1 => '<'
              case 2 => '^'
            }
          case '>' => 
            cart.turnState match {
              case 0 => '^'
              case 1 => '>'
              case 2 => 'v'
            }
        }
        cart.turnState = (cart.turnState + 1) % 3
      case _ => // Straight track, no change
    }
  }
}
