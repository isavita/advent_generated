import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8').trim();
console.log(jumbledSevenSegment(input));

function jumbledSevenSegment(input: string): number {
    const parsedInput: string[][] = [];
    for (let i = 0; i < input.split('\n').length; i++) {
        const line = input.split('\n')[i];
        const parts = line.match(/[a-g]+/g) || [];

        if (parts.length !== 14) {
            throw new Error(`should be 14 parts in each input line, got ${parts.length} for line ${i}`);
        }

        const fourteen = parts.map(alphabetizeString);
        parsedInput.push(fourteen);
    }

    let ans = 0;
    const indexToCharacters: string[] = new Array(10);
    for (const set of parsedInput) {
        let workingSet = set.slice(0, 10);
        let killIndices: number[] = [];

        for (let i = 0; i < workingSet.length; i++) {
            const mapping = workingSet[i];
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
        }

        workingSet = removeSliceIndices(workingSet, ...killIndices);

        let zeroThreeOrNine: string[] = [];
        killIndices = [];
        for (let i = 0; i < workingSet.length; i++) {
            const mapping = workingSet[i];
            if (checkStringOverlap(mapping, indexToCharacters[1])) {
                zeroThreeOrNine.push(mapping);
                killIndices.push(i);
            }
        }
        if (zeroThreeOrNine.length !== 3) {
            throw new Error(`one three or nine does not have three matches: got ${zeroThreeOrNine.length}`);
        }

        for (let i = 0; i < zeroThreeOrNine.length; i++) {
            const maybe039 = zeroThreeOrNine[i];
            if (maybe039.length === 5) {
                indexToCharacters[3] = maybe039;
                zeroThreeOrNine = removeSliceIndices(zeroThreeOrNine, i);
                break;
            }
        }

        for (let i = 0; i < zeroThreeOrNine.length; i++) {
            const maybe09 = zeroThreeOrNine[i];
            if (checkStringOverlap(maybe09, indexToCharacters[4])) {
                indexToCharacters[9] = maybe09;
                zeroThreeOrNine = removeSliceIndices(zeroThreeOrNine, i);
            }
        }

        indexToCharacters[0] = zeroThreeOrNine[0];

        workingSet = removeSliceIndices(workingSet, ...killIndices);
        if (workingSet.length !== 3) {
            throw new Error(`expected length of 3 at this stage, got ${workingSet.length}`);
        }

        for (let i = 0; i < workingSet.length; i++) {
            const mapping = workingSet[i];
            if (mapping.length === 6) {
                indexToCharacters[6] = mapping;
                workingSet = removeSliceIndices(workingSet, i);
            }
        }

        for (let i = 0; i < workingSet.length; i++) {
            const mapping = workingSet[i];
            if (checkStringOverlap(indexToCharacters[9], mapping)) {
                indexToCharacters[5] = mapping;
                workingSet = removeSliceIndices(workingSet, i);
            }
        }

        if (workingSet.length !== 1) {
            throw new Error(`expected length of 1 at this stage, got ${workingSet.length}`);
        }

        indexToCharacters[2] = workingSet[0];

        let num = 0;
        for (const out of set.slice(10)) {
            for (let i = 0; i < indexToCharacters.length; i++) {
                const mapping = indexToCharacters[i];
                if (out === mapping) {
                    num *= 10;
                    num += i;
                }
            }
        }
        ans += num;
    }

    return ans;
}

function removeSliceIndices(sli: string[], ...indices: number[]): string[] {
    const m: { [key: number]: boolean } = {};
    for (const v of indices) {
        m[v] = true;
    }

    const ans: string[] = [];
    for (let i = 0; i < sli.length; i++) {
        if (!m[i]) {
            ans.push(sli[i]);
        }
    }
    return ans;
}

function checkStringOverlap(larger: string, smaller: string): boolean {
    if (larger.length < smaller.length) {
        [larger, smaller] = [smaller, larger];
    }

    const largeMap: { [key: string]: boolean } = {};
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

function alphabetizeString(input: string): string {
    return input.split('').sort().join('');
}