
def nums = []
new File("input.txt").eachLine { line, index ->
    nums << [pos: index, val: line.toInteger()]
}
def nums2 = nums.collect { [pos: it.pos, val: 811589153 * it.val] }

mix(nums)
println(coords(nums))

def mix(nums) {
    def n = nums.size() - 1
    nums.each { num ->
        def oldpos = num.pos
        def newpos = ((oldpos + num.val) % n + n) % n
        if (oldpos < newpos) {
            nums.each { jnum ->
                if (jnum.pos > oldpos && jnum.pos <= newpos) {
                    jnum.pos--
                }
            }
        }
        if (newpos < oldpos) {
            nums.each { jnum ->
                if (jnum.pos >= newpos && jnum.pos < oldpos) {
                    jnum.pos++
                }
            }
        }
        num.pos = newpos
    }
}

def coords(nums) {
    def l = nums.size()
    def zeroPos = nums.find { it.val == 0 }.pos
    def sum = 0
    nums.each { num ->
        if (num.pos == (zeroPos + 1000) % l || num.pos == (zeroPos + 2000) % l || num.pos == (zeroPos + 3000) % l) {
            sum += num.val
        }
    }
    return sum
}
