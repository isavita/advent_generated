def input = new File("input.txt").text.trim()

def number = 1
def answer5 = ""
def answer6 = ""

while (answer5 == "" || answer6 == "") {
    def hash = (input + number).toString().md5()
    if (hash.startsWith("00000") && answer5 == "") {
        answer5 = number
    }
    if (hash.startsWith("000000") && answer6 == "") {
        answer6 = number
    }
    number++
}

println answer5
println answer6