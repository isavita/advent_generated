import * as fs from 'fs';

const readInput = (filePath: string): number[] => {
    const data = fs.readFileSync(filePath, 'utf-8').trim();
    return data.split(' ').map(Number);
};

const sumMetadata = (data: number[], index: number = 0): { sum: number; nextIndex: number } => {
    const childCount = data[index++];
    const metadataCount = data[index++];
    let total = 0;

    for (let i = 0; i < childCount; i++) {
        const result = sumMetadata(data, index);
        total += result.sum;
        index = result.nextIndex;
    }

    for (let i = 0; i < metadataCount; i++) {
        total += data[index++];
    }

    return { sum: total, nextIndex: index };
};

const main = () => {
    const input = readInput('input.txt');
    const { sum } = sumMetadata(input);
    console.log(sum);
};

main();