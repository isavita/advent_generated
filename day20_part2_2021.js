const fs = require('fs');

const iterations = 50;
const expandBy = 1;

function readInput(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const algorithm = data[0];
    const image = data.slice(2).map(row => row.trim().split('').map(char => char === '#'));
    return [algorithm, image];
}

function enhanceImage(algorithm, image, useInfiniteLit) {
    const newImage = new Array(image.length + (expandBy * 2)).fill(null).map(() => new Array(image[0].length + (expandBy * 2)).fill(false));

    for (let y = -expandBy; y < image.length + expandBy; y++) {
        for (let x = -expandBy; x < image[0].length + expandBy; x++) {
            let index = 0;
            for (let dy = -1; dy <= 1; dy++) {
                for (let dx = -1; dx <= 1; dx++) {
                    index <<= 1;
                    const ny = y + dy;
                    const nx = x + dx;
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

function countLitPixels(image) {
    return image.reduce((count, row) => count + row.filter(pixel => pixel).length, 0);
}

const [algorithm, image] = readInput('input.txt');
let currentImage = image;
for (let i = 0; i < iterations; i++) {
    currentImage = enhanceImage(algorithm, currentImage, i % 2 === 1 && algorithm[0] === '#');
}
console.log(countLitPixels(currentImage));