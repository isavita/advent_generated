
class BridgeRepair {

    static boolean solve(long target, List<Long> nums) {
        if (nums.size() == 1) {
            return nums[0] == target
        }

        for (int i = 0; i < (1 << (nums.size() - 1)); i++) {
            long result = nums[0]
            for (int j = 0; j < nums.size() - 1; j++) {
                if ((i >> j) & 1) {
                    result += nums[j + 1]
                } else {
                    result *= nums[j + 1]
                }
            }
            if (result == target) {
                return true
            }
        }
        return false
    }

    static void main(String[] args) {
        def sum = 0
        new File('input.txt').eachLine { line ->
            def parts = line.split(': ')
            def target = parts[0].toLong()
            def nums = parts[1].split(' ').collect { it.toLong() }

            if (solve(target, nums)) {
                sum += target
            }
        }
        println sum
    }
}
