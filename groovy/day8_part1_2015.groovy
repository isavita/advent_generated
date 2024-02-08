def input = new File("input.txt").text

def codeLength = input.readLines().collect { it.length() }.sum()
def memoryLength = input.readLines().collect { it.replaceAll(/\\\\|\\\"|\\x[0-9a-fA-F]{2}/, "X").length() - 2 }.sum()

println codeLength - memoryLength