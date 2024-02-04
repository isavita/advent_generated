const fs = require('fs');

const containers = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);
const target = 150;

function countCombinations(containers, target) {
    let count = 0;

    function backtrack(index, sum, numContainers) {
        if (sum === target) {
            if (numContainers >= 0) {
                count++;
            }
            return;
        }
        if (sum > target || index === containers.length) {
            return;
        }

        backtrack(index + 1, sum + containers[index], numContainers + 1);
        backtrack(index + 1, sum, numContainers);
    }

    backtrack(0, 0, 0);

    return count;
}

const totalCombinations = countCombinations(containers, target);
console.log(totalCombinations);

function minContainerCombinations(containers, target) {
    let minContainers = Infinity;
    let minCount = 0;

    function backtrack(index, sum, numContainers) {
        if (sum === target) {
            if (numContainers < minContainers) {
                minContainers = numContainers;
                minCount = 1;
            } else if (numContainers === minContainers) {
                minCount++;
            }
            return;
        }
        if (sum > target || index === containers.length) {
            return;
        }

        backtrack(index + 1, sum + containers[index], numContainers + 1);
        backtrack(index + 1, sum, numContainers);
    }

    backtrack(0, 0, 0);

    return minCount;
}

const minContainers = minContainerCombinations(containers, target);
console.log(minContainers);