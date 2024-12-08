
def input = new File("input.txt").text

def re = /(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))/

def matches = input.findAll(re)

def enabled = true
def totalSum = 0

matches.each { match ->
    if (match =~ /mul\(\d{1,3},\d{1,3}\)/) {
        if (enabled) {
            def nums = match.substring(4, match.length() - 1).split(",")
            totalSum += Integer.parseInt(nums[0]) * Integer.parseInt(nums[1])
        }
    } else if (match == "do()") {
        enabled = true
    } else if (match == "don't()") {
        enabled = false
    }
}

println totalSum
