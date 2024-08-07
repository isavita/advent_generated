const iterations = 50;
const expandBy = 1;

function main() {
    let [algorithm, image] = readInput("input.txt");
    for (let i = 0; i < iterations; i++) {
        image = enhanceImage(algorithm, image, i % 2 === 1 && algorithm[0] === '#');
    }
    console.log(countLitPixels(image));
}

function readInput(filename: string): [string, boolean[][]] {
    const fs = require('fs');
    const data = fs.readFileSync(filename, 'utf-8').split('\n');
    const algorithm = data[0];
    const image: boolean[][] = [];

    for (let i = 2; i < data.length; i++) {
        const line = data[i].trim();
        if (line) {
            const row = line.split('').map((char: string) => char === '#');
            image.push(row);
        }
    }

    return [algorithm, image];
}

function enhanceImage(algorithm: string, image: boolean[][], useInfiniteLit: boolean): boolean[][] {
    const newImage: boolean[][] = [];
    for (let i = 0; i < image.length + (expandBy * 2); i++) {
        newImage.push(new Array(image[0].length + (expandBy * 2)).fill(false));
    }

    for (let y = -expandBy; y < image.length + expandBy; y++) {
        for (let x = -expandBy; x < image[0].length + expandBy; x++) {
            let index = 0;
            for (let dy = -1; dy <= 1; dy++) {
                for (let dx = -1; dx <= 1; dx++) {
                    index <<= 1;
                    const ny = y + dy, nx = x + dx;
                    if (ny >= 0 && ny < image.length && nx >= 0 && nx < image[0].length) {
                        if (image[ny][nx]) {
                            index |= 1;
                        }
                    } else if (useInfiniteLit) {
                        index |= 1;
                    }
                }
            }
            newImage[y + expandBy][x + expandBy] = algorithm[index] === '#';
        }
    }

    return newImage;
}

function countLitPixels(image: boolean[][]): number {
    let count = 0;
    for (const row of image) {
        for (const pixel of row) {
            if (pixel) {
                count++;
            }
        }
    }
    return count;
}

main();