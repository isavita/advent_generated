
def initialState = new File("input.txt").text.trim()
def diskLength = 35651584

def data = generateData(initialState, diskLength)
def checksum = calculateChecksum(data)
println "Checksum: $checksum"

String generateData(String initialState, int length) {
    def data = initialState
    while (data.size() < length) {
        def b = new StringBuilder()
        for (int i = data.size() - 1; i >= 0; i--) {
            if (data[i] == '0') {
                b.append('1')
            } else {
                b.append('0')
            }
        }
        data += "0" + b.toString()
    }
    return data.substring(0, length)
}

String calculateChecksum(String data) {
    while (data.size() % 2 == 0) {
        def b = new StringBuilder()
        for (int i = 0; i < data.size(); i += 2) {
            if (data[i] == data[i + 1]) {
                b.append('1')
            } else {
                b.append('0')
            }
        }
        data = b.toString()
    }
    return data
}
