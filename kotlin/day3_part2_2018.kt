import java.io.File

data class Claim(val ID: Int, val X: Int, val Y: Int, val Width: Int, val Height: Int)

fun main(args: Array<String>) {
    val claims = readClaims("input.txt")
    
    val fabric = Array(1000) { IntArray(1000) }
    
    for (claim in claims) {
        for (y in claim.Y until claim.Y + claim.Height) {
            for (x in claim.X until claim.X + claim.Width) {
                fabric[y][x]++
            }
        }
    }
    
    for (claim in claims) {
        var overlap = false
        for (y in claim.Y until claim.Y + claim.Height) {
            for (x in claim.X until claim.X + claim.Width) {
                if (fabric[y][x] > 1) {
                    overlap = true
                    break
                }
            }
            if (overlap) {
                break
            }
        }
        if (!overlap) {
            println(claim.ID)
            return
        }
    }
}

fun readClaims(filename: String): List<Claim> {
    val file = File(filename)
    val claims = mutableListOf<Claim>()
    
    file.forEachLine {
        val parts = it.split(" ")
        val id = parts[0].substring(1).toInt()
        val coords = parts[2].substring(0, parts[2].length - 1).split(",")
        val x = coords[0].toInt()
        val y = coords[1].toInt()
        val dims = parts[3].split("x")
        val width = dims[0].toInt()
        val height = dims[1].toInt()
        claims.add(Claim(id, x, y, width, height))
    }
    
    return claims
}