def input = new File('input.txt').text.trim()
def bytes = input.bytes
def suffix = [17, 31, 73, 47, 23]
def lengths = bytes.collect { it } + suffix

def list = (0..255).toList()
def currentPos = 0
def skipSize = 0

64.times {
    lengths.each { length ->
        def sublist = []
        for (int i = 0; i < length; i++) {
            sublist.add(list[(currentPos + i) % list.size()])
        }
        sublist = sublist.reverse()
        for (int i = 0; i < length; i++) {
            list[(currentPos + i) % list.size()] = sublist[i]
        }
        currentPos = (currentPos + length + skipSize) % list.size()
        skipSize++
    }
}

def sparseHash = list
def denseHash = []
for (int i = 0; i < 16; i++) {
    def block = sparseHash.subList(i * 16, (i + 1) * 16)
    def xor = block.inject(0) { acc, val -> acc ^ val }
    denseHash.add(xor)
}

def hexString = denseHash.collect { String.format('%02x', it) }.join()
println hexString