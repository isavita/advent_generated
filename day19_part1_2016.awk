BEGIN {
    getline totalElves < "input.txt"
    highestPowerOfTwo = 1
    while (highestPowerOfTwo * 2 <= totalElves) {
        highestPowerOfTwo *= 2
    }
    winner = (totalElves - highestPowerOfTwo) * 2 + 1
    print winner
}