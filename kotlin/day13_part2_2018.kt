import java.io.File

data class Cart(var x: Int, var y: Int, var direction: Char, var turns: Int)

fun main() {
    val input = File("input.txt").readLines()
    val tracks = mutableListOf<CharArray>()
    val carts = mutableListOf<Cart>()

    input.forEachIndexed { y, line ->
        val trackLine = CharArray(line.length)
        line.forEachIndexed { x, r ->
            when (r) {
                '>', '<', '^', 'v' -> {
                    carts.add(Cart(x, y, r, 0))
                    trackLine[x] = if (r == '>' || r == '<') '-' else '|'
                }
                else -> trackLine[x] = r
            }
        }
        tracks.add(trackLine)
    }

    while (carts.size > 1) {
        carts.sortWith(compareBy({ it.y }, { it.x }))
        val toRemove = mutableSetOf<Int>()

        carts.forEachIndexed { i, cart ->
            if (i in toRemove) return@forEachIndexed
            moveCart(cart, tracks)
            carts.indexOfFirst { it != cart && it.x == cart.x && it.y == cart.y }.takeIf { it != -1 }?.let {
                toRemove.add(i)
                toRemove.add(it)
            }
        }

        carts.removeAll { toRemove.contains(carts.indexOf(it)) }
    }

    println("${carts[0].x},${carts[0].y}")
}

fun moveCart(cart: Cart, tracks: List<CharArray>) {
    when (cart.direction) {
        '>' -> cart.x++
        '<' -> cart.x--
        '^' -> cart.y--
        'v' -> cart.y++
    }
    when (tracks[cart.y][cart.x]) {
        '+' -> turnCart(cart)
        '/', '\\' -> changeDirection(cart, tracks[cart.y][cart.x])
    }
}

fun turnCart(cart: Cart) {
    when (cart.turns % 3) {
        0 -> cart.direction = when (cart.direction) {
            '>' -> '^'
            '<' -> 'v'
            '^' -> '<'
            else -> '>'
        }
        2 -> cart.direction = when (cart.direction) {
            '>' -> 'v'
            '<' -> '^'
            '^' -> '>'
            else -> '<'
        }
    }
    cart.turns++
}

fun changeDirection(cart: Cart, track: Char) {
    cart.direction = when {
        track == '/' && cart.direction == '>' -> '^'
        track == '/' && cart.direction == '<' -> 'v'
        track == '/' && cart.direction == '^' -> '>'
        track == '/' && cart.direction == 'v' -> '<'
        track == '\\' && cart.direction == '>' -> 'v'
        track == '\\' && cart.direction == '<' -> '^'
        track == '\\' && cart.direction == '^' -> '<'
        track == '\\' && cart.direction == 'v' -> '>'
        else -> cart.direction
    }
}