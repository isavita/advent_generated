
{
    steps = $0
    currentPos = 0
    valueAfterZero = 0

    for (i = 1; i <= 50000000; i++) {
        currentPos = (currentPos + steps) % i
        if (currentPos == 0) {
            valueAfterZero = i
        }
        currentPos++
    }

    print valueAfterZero
}
