def input = new File("input.txt").text.readLines()

def result = ""

for (int i = 0; i < input[0].size(); i++) {
    def charMap = [:].withDefault{ 0 }
    input.each { line ->
        charMap[line[i]]++
    }
    result += charMap.max{ it.value }.key
}

println result