
{
    steps = $1
}
END {
    buffer[0] = 0
    currentPos = 0
    
    for (i = 1; i <= 2017; i++) {
        currentPos = (currentPos + steps) % length(buffer)
        for (j = length(buffer); j > currentPos + 1; j--) {
            buffer[j] = buffer[j-1]
        }
        buffer[currentPos+1] = i
        currentPos++
    }
    
    for (k in buffer) {
        if (buffer[k] == 2017) {
            print buffer[(k+1)%length(buffer)]
            break
        }
    }
}
