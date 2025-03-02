
def mix(nums) {
    def n = nums.size() - 1
    (0..<nums.size()).each { i ->
        def oldpos = nums[i][0]
        def newpos = ((oldpos + nums[i][1]) % n + n) % n
        if (oldpos < newpos) {
            (0..<nums.size()).each { j ->
                if (nums[j][0] > oldpos && nums[j][0] <= newpos) {
                    nums[j][0] -= 1
                }
            }
        }
        if (newpos < oldpos) {
            (0..<nums.size()).each { j ->
                if (nums[j][0] >= newpos && nums[j][0] < oldpos) {
                    nums[j][0] += 1
                }
            }
        }
        nums[i][0] = newpos
    }
}

def coords(nums) {
    def l = nums.size()
    def zero_pos = 0
    (0..<nums.size()).each { i ->
        if (nums[i][1] == 0) {
            zero_pos = nums[i][0]
        }
    }
    def total_sum = 0
    (0..<nums.size()).each { i ->
        if (nums[i][0] == (zero_pos + 1000) % l || nums[i][0] == (zero_pos + 2000) % l || nums[i][0] == (zero_pos + 3000) % l) {
            total_sum += nums[i][1]
        }
    }
    return total_sum
}

def static main(args) {
    def lines = new File("input.txt").readLines()
    def nums = lines.indexed().collect { idx, val -> [idx, val.toInteger()] }
    def nums2 = nums.collect { [it[0], 811589153L * it[1]] }

    (0..<10).each {
        mix(nums2)
    }
    println coords(nums2)
}
