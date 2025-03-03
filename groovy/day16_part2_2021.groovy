
def hexToBinary(String hex) {
    hex.toUpperCase().collect {
        switch (it) {
            case '0': '0000'; break
            case '1': '0001'; break
            case '2': '0010'; break
            case '3': '0011'; break
            case '4': '0100'; break
            case '5': '0101'; break
            case '6': '0110'; break
            case '7': '0111'; break
            case '8': '1000'; break
            case '9': '1001'; break
            case 'A': '1010'; break
            case 'B': '1011'; break
            case 'C': '1100'; break
            case 'D': '1101'; break
            case 'E': '1110'; break
            case 'F': '1111'; break
            default: ''
        }
    }.join()
}

def parsePacket(String binary) {
    int version = Integer.parseInt(binary.substring(0, 3), 2)
    int typeId = Integer.parseInt(binary.substring(3, 6), 2)
    String remaining = binary.substring(6)

    if (typeId == 4) { // Literal value
        StringBuilder literalValueBinary = new StringBuilder()
        int i = 0
        while (true) {
            literalValueBinary.append(remaining.substring(i + 1, i + 5))
            if (remaining.charAt(i) == '0') {
                remaining = remaining.substring(i + 5)
                break
            }
            i += 5
        }
        long literalValue = Long.parseLong(literalValueBinary.toString(), 2)
        return [version, typeId, literalValue, remaining]
    } else { // Operator
        int lengthTypeId = Integer.parseInt(remaining.substring(0, 1), 2)
        remaining = remaining.substring(1)

        List<Object> subPacketValues = []
        if (lengthTypeId == 0) {
            int totalLength = Integer.parseInt(remaining.substring(0, 15), 2)
            remaining = remaining.substring(15)
            String subPacketsBinary = remaining.substring(0, totalLength)
            remaining = remaining.substring(totalLength)

            while (subPacketsBinary.length() > 6 && subPacketsBinary.find { it == '1' }) {
                def result = parsePacket(subPacketsBinary)
                version += result[0]
                subPacketsBinary = result[3]
                subPacketValues.add(result[2])
            }
        } else {
            int numSubPackets = Integer.parseInt(remaining.substring(0, 11), 2)
            remaining = remaining.substring(11)

            for (int i = 0; i < numSubPackets; i++) {
                def result = parsePacket(remaining)
                version += result[0]
                remaining = result[3]
                subPacketValues.add(result[2])
            }
        }

        def packetValue
        switch (typeId) {
            case 0: packetValue = subPacketValues.sum(); break
            case 1: packetValue = subPacketValues.inject(1) { acc, val -> acc * val }; break
            case 2: packetValue = subPacketValues.min(); break
            case 3: packetValue = subPacketValues.max(); break
            case 5: packetValue = subPacketValues[0] > subPacketValues[1] ? 1 : 0; break
            case 6: packetValue = subPacketValues[0] < subPacketValues[1] ? 1 : 0; break
            case 7: packetValue = subPacketValues[0] == subPacketValues[1] ? 1 : 0; break
        }

        return [version, typeId, packetValue, remaining]
    }
}

def solve() {
    File inputFile = new File("input.txt")
    String hexInput = inputFile.text.trim()
    String binaryInput = hexToBinary(hexInput)

    def result = parsePacket(binaryInput)
    println "Part 1: Version sum = ${result[0]}"
    println "Part 2: Packet value = ${result[2]}"
}

solve()
