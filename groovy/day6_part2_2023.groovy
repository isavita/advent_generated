import java.math.BigInteger

def calculateWaysToWinLongRace(time, record) {
    waysToWin = 0
    for (holdTime in 1..<time) {
        travelTime = time - holdTime
        distance = holdTime * travelTime
        if (distance > record) {
            waysToWin++
        }
    }
    waysToWin
}

file = new File("input.txt")
time = new BigInteger("0")
distance = new BigInteger("0")

file.eachLine { line ->
    if (line.trim() != "") {
        parts = line.split(":")
        value = parts[1].replaceAll("\\s", "")
        if (time == BigInteger.ZERO) {
            time = new BigInteger(value)
        } else {
            distance = new BigInteger(value)
        }
    }
}

waysToWin = calculateWaysToWinLongRace(time, distance)
println waysToWin