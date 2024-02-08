def data = new File("input.txt").text.trim()

while (data.size() < 272) {
    def a = data
    def b = a.reverse().collect {
        it == '0' ? '1' : '0'
    }.join()
    data = "${a}0${b}"
}

data = data[0..271] // Keep only the first 272 characters

while (data.size() % 2 == 0) {
    def checksum = ""
    for (int i = 0; i < data.size(); i += 2) {
        checksum += data[i] == data[i + 1] ? '1' : '0'
    }
    data = checksum
}

println data