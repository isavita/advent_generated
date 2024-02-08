
def input = new File("input.txt").text.split("-").collect { it.toInteger() }
def count = (input[0]..input[1]).count { num ->
    def strNum = num.toString()
    def adjacent = false
    def increasing = true
    for (int i = 0; i < strNum.size() - 1; i++) {
        if (strNum[i] == strNum[i + 1]) {
            adjacent = true
        }
        if (strNum[i] > strNum[i + 1]) {
            increasing = false
            break
        }
    }
    adjacent && increasing
}
println count
