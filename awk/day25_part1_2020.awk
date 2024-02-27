
BEGIN {
    FS = " "
    getline cardPublicKey < "input.txt"
    getline doorPublicKey < "input.txt"
    
    value = 1
    loopSize = 0
    while (value != cardPublicKey) {
        value = (value * 7) % 20201227
        loopSize++
    }
    
    encryptionKey = 1
    for (i = 0; i < loopSize; i++) {
        encryptionKey = (encryptionKey * doorPublicKey) % 20201227
    }
    
    print encryptionKey
}
