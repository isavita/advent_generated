
def input = new File("input.txt").readLines().collect { it.toLong() }
def preamble = 25

def findInvalidNumber = { list, index ->
    def number = list[index]
    def preambleList = list.subList(index - preamble, index)
    !preambleList.any { num ->
        def complement = number - num
        complement != num && preambleList.contains(complement)
    }
}

def invalidNumberIndex = (preamble..input.size()-1).find { findInvalidNumber(input, it) }
def invalidNumber = input[invalidNumberIndex]

println invalidNumber
