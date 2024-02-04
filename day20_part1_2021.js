const fs = require('fs');

function readInput(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const algorithm = data[0].replace('\n', '');
    const image = data.slice(2).map(row => row.split(''));
    return [algorithm, image];
}

function enhanceImage(image, algorithm, times) {
    for (let i = 0; i < times; i++) {
        image = applyAlgorithm(image, algorithm, i % 2 === 1 && algorithm[0] === '#');
    }
    return image;
}

function applyAlgorithm(image, algorithm, flip) {
    const enhancedImage = new Array(image.length + 2).fill(null).map(() => new Array(image[0].length + 2).fill(null));
    for (let i = 0; i < enhancedImage.length; i++) {
        for (let j = 0; j < enhancedImage[i].length; j++) {
            const index = calculateIndex(i - 1, j - 1, image, flip);
            enhancedImage[i][j] = algorithm[index];
        }
    }
    return enhancedImage;
}

function calculateIndex(i, j, image, flip) {
    let index = 0;
    for (let di = -1; di <= 1; di++) {
        for (let dj = -1; dj <= 1; dj++) {
            index <<= 1;
            if (i + di >= 0 && i + di < image.length && j + dj >= 0 && j + dj < image[0].length) {
                if (image[i + di][j + dj] === '#') {
                    index |= 1;
                }
            } else if (flip) {
                index |= 1;
            }
        }
    }
    return index;
}

function countLitPixels(image) {
    let count = 0;
    image.forEach(row => {
        row.forEach(pixel => {
            if (pixel === '#') {
                count++;
            }
        });
    });
    return count;
}

const [algorithm, image] = readInput("input.txt");
const enhancedImage = enhanceImage(image, algorithm, 2);
console.log(countLitPixels(enhancedImage));