
def containers = new File("input.txt").readLines().collect { it as int }

def findCombinations(containers, target, index, count, minCount, ways) {
	if (target == 0) {
		if (minCount[0] == 0 || count < minCount[0]) {
			minCount[0] = count
			ways[0] = 1
		} else if (count == minCount[0]) {
			ways[0]++
		}
		return
	}
	if (target < 0 || index >= containers.size()) {
		return
	}
	findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways)
	findCombinations(containers, target, index + 1, count, minCount, ways)
}

def minCount = [0]
def ways = [0]
findCombinations(containers, 150, 0, 0, minCount, ways)
println ways[0]
