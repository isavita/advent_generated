import java.io.File
import java.util.*

fun main() {
    val input = File("input.txt").readLines()

    val tracks = input.map { it.toCharArray() }.toTypedArray()
    val carts = mutableListOf<Cart>()

    for (i in tracks.indices) {
        for (j in tracks[i].indices) {
            when (tracks[i][j]) {
                '^' -> carts.add(Cart(j, i, Direction.UP))
                'v' -> carts.add(Cart(j, i, Direction.DOWN))
                '<' -> carts.add(Cart(j, i, Direction.LEFT))
                '>' -> carts.add(Cart(j, i, Direction.RIGHT))
            }
        }
    }

    while (true) {
        carts.sort()

        val crashedCarts = mutableSetOf<Cart>()

        for (cart in carts) {
            if (crashedCarts.contains(cart)) continue

            cart.move(tracks)
            val collidingCart = carts.find { it != cart && it.x == cart.x && it.y == cart.y }

            if (collidingCart != null) {
                println("${cart.x},${cart.y}")
                return
            }
        }
    }
}

data class Cart(var x: Int, var y: Int, var direction: Direction, var nextTurn: Turn = Turn.LEFT) : Comparable<Cart> {
    override fun compareTo(other: Cart): Int {
        if (y != other.y) {
            return y - other.y
        }
        return x - other.x
    }

    fun move(tracks: Array<CharArray>) {
        when (direction) {
            Direction.UP -> y--
            Direction.DOWN -> y++
            Direction.LEFT -> x--
            Direction.RIGHT -> x++
        }

        val track = tracks[y][x]

        when (track) {
            '/' -> direction = when (direction) {
                Direction.UP -> Direction.RIGHT
                Direction.DOWN -> Direction.LEFT
                Direction.LEFT -> Direction.DOWN
                Direction.RIGHT -> Direction.UP
            }
            '\\' -> direction = when (direction) {
                Direction.UP -> Direction.LEFT
                Direction.DOWN -> Direction.RIGHT
                Direction.LEFT -> Direction.UP
                Direction.RIGHT -> Direction.DOWN
            }
            '+' -> {
                when (nextTurn) {
                    Turn.LEFT -> {
                        direction = turnLeft()
                        nextTurn = Turn.STRAIGHT
                    }
                    Turn.STRAIGHT -> nextTurn = Turn.RIGHT
                    Turn.RIGHT -> {
                        direction = turnRight()
                        nextTurn = Turn.LEFT
                    }
                }
            }
        }
    }

    private fun turnLeft(): Direction {
        return when (direction) {
            Direction.UP -> Direction.LEFT
            Direction.DOWN -> Direction.RIGHT
            Direction.LEFT -> Direction.DOWN
            Direction.RIGHT -> Direction.UP
        }
    }

    private fun turnRight(): Direction {
        return when (direction) {
            Direction.UP -> Direction.RIGHT
            Direction.DOWN -> Direction.LEFT
            Direction.LEFT -> Direction.UP
            Direction.RIGHT -> Direction.DOWN
        }
    }
}

enum class Direction {
    UP, DOWN, LEFT, RIGHT
}

enum class Turn {
    LEFT, STRAIGHT, RIGHT
}