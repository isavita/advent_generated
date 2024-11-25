
class Solution {
  construct(filename) {
    this.containers = File.read(filename).lines.map {|line| line.toNumber() }
  }

  combinations(target, index, currentCombination) {
    if target == 0 {
      return 1;
    }
    if target < 0 || index >= this.containers.count {
      return 0;
    }

    let count = 0;
    count += this.combinations(target - this.containers[index], index + 1, currentCombination + [this.containers[index]]);
    count += this.combinations(target, index + 1, currentCombination);
    return count;
  }

  minContainers(target) {
    var minCount = -1;
    var countAtMin = 0;

    for (i in 0..this.containers.count) {
      let combinations = this.combinationsWithCount(target, 0, [], i);
      if (combinations.count > 0) {
        if (minCount == -1 || i < minCount) {
          minCount = i;
          countAtMin = combinations.count;
        } else if (i == minCount) {
          countAtMin += combinations.count;
        }
      }
    }
    return countAtMin;
  }

  combinationsWithCount(target, index, currentCombination, minCount) {
    if (target == 0 && currentCombination.count == minCount) {
      return [currentCombination];
    }
    if (target < 0 || index >= this.containers.count || currentCombination.count > minCount) {
      return [];
    }

    var result = [];
    result.addAll(this.combinationsWithCount(target - this.containers[index], index + 1, currentCombination + [this.containers[index]], minCount));
    result.addAll(this.combinationsWithCount(target, index + 1, currentCombination, minCount));
    return result;
  }

  solve() {
    let totalCombinations = this.combinations(150, 0, []);
    let minContainersCombinations = this.minContainers(150);
    IO.print("Part 1: ", totalCombinations);
    IO.print("Part 2: ", minContainersCombinations);
  }
}

Solution("input.txt").solve();
