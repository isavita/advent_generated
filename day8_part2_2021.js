const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

function jumbledSevenSegment(input) {
    const parsedInput = input.split('\n').map(line => {
        const parts = line.match(/[a-g]+/g);
        if (parts.length !== 14) {
            throw new Error(`should be 14 parts in each input line, got ${parts.length}`);
        }
        return parts.map(part => alphabetizeString(part));
    });

    let ans = 0;
    const indexToCharacters = new Array(10).fill('');

    parsedInput.forEach(set => {
        let workingSet = set.slice(0, 10);
        let killIndices = [];

        workingSet.forEach((mapping, i) => {
            switch (mapping.length) {
                case 2:
                    indexToCharacters[1] = mapping;
                    killIndices.push(i);
                    break;
                case 4:
                    indexToCharacters[4] = mapping;
                    killIndices.push(i);
                    break;
                case 3:
                    indexToCharacters[7] = mapping;
                    killIndices.push(i);
                    break;
                case 7:
                    indexToCharacters[8] = mapping;
                    killIndices.push(i);
                    break;
            }
        });

        workingSet = removeSliceIndices(workingSet, killIndices);

        let zeroThreeOrNine = [];
        killIndices = [];
        workingSet.forEach((mapping, i) => {
            if (checkStringOverlap(mapping, indexToCharacters[1])) {
                zeroThreeOrNine.push(mapping);
                killIndices.push(i);
            }
        });

        if (zeroThreeOrNine.length !== 3) {
            throw new Error(`one three or nine does not have three matches: got ${zeroThreeOrNine.length}`);
        }

        zeroThreeOrNine.forEach((maybe039, i) => {
            if (maybe039.length === 5) {
                indexToCharacters[3] = maybe039;
                zeroThreeOrNine.splice(i, 1);
            }
        });

        zeroThreeOrNine.forEach((maybe09, i) => {
            if (checkStringOverlap(maybe09, indexToCharacters[4])) {
                indexToCharacters[9] = maybe09;
                zeroThreeOrNine.splice(i, 1);
            }
        });

        indexToCharacters[0] = zeroThreeOrNine[0];

        workingSet = removeSliceIndices(workingSet, killIndices);

        if (workingSet.length !== 3) {
            throw new Error(`expected length of 3 at this stage, got ${workingSet.length}`);
        }

        workingSet.forEach((mapping, i) => {
            if (mapping.length === 6) {
                indexToCharacters[6] = mapping;
                workingSet.splice(i, 1);
            }
        });

        workingSet.forEach((mapping, i) => {
            if (checkStringOverlap(indexToCharacters[9], mapping)) {
                indexToCharacters[5] = mapping;
                workingSet.splice(i, 1);
            }
        });

        if (workingSet.length !== 1) {
            throw new Error(`expected length of 1 at this stage, got ${workingSet.length}`);
        }

        indexToCharacters[2] = workingSet[0];

        let num = 0;
        set.slice(10).forEach(out => {
            indexToCharacters.forEach((mapping, i) => {
                if (out === mapping) {
                    num *= 10;
                    num += i;
                }
            });
        });

        ans += num;
    });

    return ans;
}

function removeSliceIndices(sli, indices) {
    const m = {};
    indices.forEach(v => {
        m[v] = true;
    });

    return sli.filter((_, i) => !m[i]);
}

function checkStringOverlap(larger, smaller) {
    if (larger.length < smaller.length) {
        [larger, smaller] = [smaller, larger];
    }

    const largeMap = {};
    for (const r of larger) {
        largeMap[r] = true;
    }

    for (const r of smaller) {
        if (!largeMap[r]) {
            return false;
        }
    }

    return true;
}

function alphabetizeString(input) {
    const chars = input.split('').sort();
    return chars.join('');
}

const ans = jumbledSevenSegment(input);
console.log(ans);