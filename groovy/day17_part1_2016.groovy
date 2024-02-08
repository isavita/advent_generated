
def passcode = new File("input.txt").text.trim()
def queue = [[0, 0, ""]]
def dirs = ["U", "D", "L", "R"]

while (queue) {
    def point = queue.remove(0)
    if (point[0] == 3 && point[1] == 3) {
        println point[2]
        break
    }
    def hash = (passcode + point[2]).getBytes("UTF-8") as byte[]
    def md5 = java.security.MessageDigest.getInstance("MD5").digest(hash).encodeHex().toString()

    dirs.eachWithIndex { dir, idx ->
        if (md5[idx] >= 'b' && md5[idx] <= 'f') {
            def nextPoint = [point[0] + (dir == "L" ? -1 : (dir == "R" ? 1 : 0)), point[1] + (dir == "U" ? -1 : (dir == "D" ? 1 : 0)), point[2] + dir]
            if (nextPoint[0] >= 0 && nextPoint[0] < 4 && nextPoint[1] >= 0 && nextPoint[1] < 4) {
                queue.add(nextPoint)
            }
        }
    }
}
