
def passcode = new File('input.txt').text.trim()
def longestPathLength = findLongestPathLength(passcode)
println longestPathLength

def readPasscode(String filename) {
    new File(filename).text.trim()
}

def findLongestPathLength(String passcode) {
    def longest = 0
    def queue = [[0, 0, ""]]
    while (queue.size() > 0) {
        def point = queue.remove(0)

        if (point[0] == 3 && point[1] == 3) {
            if (point[2].size() > longest) {
                longest = point[2].size()
            }
            continue
        }

        getOpenDoors(passcode, point[2]).each { dir ->
            def nextPoint = [point[0], point[1], point[2] + dir]
            switch (dir) {
                case "U":
                    nextPoint[1]--
                    break
                case "D":
                    nextPoint[1]++
                    break
                case "L":
                    nextPoint[0]--
                    break
                case "R":
                    nextPoint[0]++
                    break
            }

            if (nextPoint[0] >= 0 && nextPoint[0] < 4 && nextPoint[1] >= 0 && nextPoint[1] < 4) {
                queue.add(nextPoint)
            }
        }
    }
    longest
}

def getOpenDoors(passcode, path) {
    def hash = md5Hash(passcode + path)
    def doors = []
    if (hash[0] >= 'b' && hash[0] <= 'f') {
        doors.add("U")
    }
    if (hash[1] >= 'b' && hash[1] <= 'f') {
        doors.add("D")
    }
    if (hash[2] >= 'b' && hash[2] <= 'f') {
        doors.add("L")
    }
    if (hash[3] >= 'b' && hash[3] <= 'f') {
        doors.add("R")
    }
    doors
}

def md5Hash(String input) {
    def md5 = java.security.MessageDigest.getInstance("MD5")
    md5.update(input.bytes)
    md5.digest().encodeHex().toString()
}
