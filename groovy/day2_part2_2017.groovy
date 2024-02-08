
def file = new File("input.txt")
def sum = 0

file.eachLine { line ->
    def nums = line.tokenize().collect { it as int }

    nums.each { num1 ->
        nums.each { num2 ->
            if (num1 != num2 && num1 % num2 == 0) {
                sum += num1 / num2
            }
        }
    }
}

println sum
