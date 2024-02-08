
def lines = new File("input.txt").readLines()

class Star {
    int x, y, vX, vY
    Star next
}

def head = new Star()
def tail = head

def re = ~/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/

lines.each { line ->
    def split = (line =~ re)[0]
    if (split.size() != 5) {
        return
    }
    def star = new Star(x: split[1] as int, y: split[2] as int, vX: split[3] as int, vY: split[4] as int)
    tail.next = star
    tail = star
}

def smallestT = 0
def smallestArea = (1 << 31) - 1

(1..100000).each { t ->
    def maxX = 0
    def maxY = 0
    def minX = 0
    def minY = 0

    def temp = head.next
    while (temp.next != null) {
        def x = temp.x + temp.vX*t
        if (maxX < x) {
            maxX = x
        } else if (minX > x) {
            minX = x
        }
        def y = temp.y + temp.vY*t
        if (maxY < y) {
            maxY = y
        } else if (minY > y) {
            minY = y
        }
        temp = temp.next
    }

    def lenX = maxX - minX + 1
    def lenY = maxY - minY + 1
    def area = lenX + lenY

    if (smallestArea > area) {
        smallestArea = area
        smallestT = t
    }
}

println smallestT

def t = smallestT

def maxX = 0
def maxY = 0
def minX = 0
def minY = 0

def temp = head.next
while (temp.next != null) {
    temp.x += temp.vX*t
    if (maxX < temp.x) {
        maxX = temp.x
    } else if (minX > temp.x) {
        minX = temp.x
    }
    temp.y += temp.vY*t
    if (maxY < temp.y) {
        maxY = temp.y
    } else if (minY > temp.y) {
        minY = temp.y
    }
    temp = temp.next
}

def mapper = new boolean[maxY - minY + 1][maxX - minX + 1]

temp = head.next
while (temp.next != null) {
    mapper[temp.y - minY][temp.x - minX] = true
    temp = temp.next
}

for (i in 0..<mapper.size()) {
    for (j in 0..<mapper[0].size()) {
    }
}
