
{
    messages[NR] = $0
}

END {
    originalMessage = ""
    messageLength = length(messages[1])

    for (i = 1; i <= messageLength; i++) {
        delete count
        for (j = 1; j <= NR; j++) {
            char = substr(messages[j], i, 1)
            count[char]++
        }
        minChar = ""
        minCount = 9999999999
        for (char in count) {
            if (count[char] < minCount) {
                minCount = count[char]
                minChar = char
            }
        }
        originalMessage = originalMessage minChar
    }

    print originalMessage
}
