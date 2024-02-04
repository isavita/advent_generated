const fs = require('fs');

class LLNode {
    constructor(elfNum, presents) {
        this.elfNum = elfNum;
        this.presents = presents;
        this.next = null;
    }
}

function elephant(input) {
    const startingElves = parseInt(input);
    let root = new LLNode(1, 1);
    let iter = root;
    for (let i = 2; i <= startingElves; i++) {
        iter.next = new LLNode(i, 1);
        iter = iter.next;
    }
    iter.next = root;

    let isOddLength = startingElves % 2 === 1;
    let beforeAcross = root;
    for (let i = 0; i < Math.floor(startingElves / 2) - 1; i++) {
        beforeAcross = beforeAcross.next;
    }

    while (root.next !== root) {
        root.presents += beforeAcross.next.presents;
        beforeAcross.next = beforeAcross.next.next;

        if (isOddLength) {
            beforeAcross = beforeAcross.next;
        }
        isOddLength = !isOddLength;
        root = root.next;
    }

    return root.elfNum;
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
const ans = elephant(input);
console.log(ans);