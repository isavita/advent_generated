
def hexToBinary(String hex) {
    return hex.collect { String.format('%4s', Integer.toBinaryString(Integer.parseInt(it, 16))).replace(' ', '0') }.join('')
}

def parsePacket(String binary, int[] index) {
    int version = Integer.parseInt(binary[index[0]..index[0] + 2], 2)
    index[0] += 3 // Move past version
    int typeId = Integer.parseInt(binary[index[0]..index[0] + 2], 2)
    index[0] += 3 // Move past type ID

    int versionSum = version

    if (typeId == 4) {
        // Literal value packet
        StringBuilder literalValue = new StringBuilder()
        boolean moreBits = true
        while (moreBits) {
            moreBits = binary[index[0]] == '1'
            literalValue.append(binary[index[0] + 1..index[0] + 4])
            index[0] += 5 // Move past this group
        }
        // The literal value is not needed for the version sum
    } else {
        // Operator packet
        int lengthTypeId = binary[index[0]]
        index[0] += 1 // Move past length type ID

        if (lengthTypeId == '0') {
            // Next 15 bits are the total length of sub-packets
            int totalLength = Integer.parseInt(binary[index[0]..index[0] + 14], 2)
            index[0] += 15 // Move past length
            int endIndex = index[0] + totalLength

            while (index[0] < endIndex) {
                versionSum += parsePacket(binary, index)
            }
        } else {
            // Next 11 bits are the number of sub-packets
            int numberOfSubPackets = Integer.parseInt(binary[index[0]..index[0] + 10], 2)
            index[0] += 11 // Move past number of sub-packets

            for (int i = 0; i < numberOfSubPackets; i++) {
                versionSum += parsePacket(binary, index)
            }
        }
    }
    return versionSum
}

def main() {
    def input = new File('input.txt').text.trim()
    def binary = hexToBinary(input)

    int[] index = [0]
    int totalVersionSum = parsePacket(binary, index)

    println "Total version sum: $totalVersionSum"
}

main()
