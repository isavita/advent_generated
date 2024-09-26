const fs = require('fs');

/**
 * Reads the input data from a given file path.
 * @param {string} filePath - Path to the input file.
 * @returns {string} - The input data as a string.
 */
function readInput(filePath) {
    try {
        return fs.readFileSync(filePath, 'utf8').trim();
    } catch (error) {
        console.error(`Error reading the file at ${filePath}:`, error);
        process.exit(1);
    }
}

/**
 * Splits the input data into layers based on image width and height.
 * @param {string} data - The input data string.
 * @param {number} width - Width of the image.
 * @param {number} height - Height of the image.
 * @returns {string[]} - An array of layer strings.
 */
function splitIntoLayers(data, width, height) {
    const layerSize = width * height;
    const layers = [];
    for (let i = 0; i < data.length; i += layerSize) {
        layers.push(data.slice(i, i + layerSize));
    }
    return layers;
}

/**
 * Finds the layer with the fewest '0' digits.
 * @param {string[]} layers - Array of layer strings.
 * @returns {string} - The layer with the fewest '0's.
 */
function findLayerWithFewestZeros(layers) {
    let fewestZeros = Infinity;
    let targetLayer = '';
    for (const layer of layers) {
        const zeroCount = (layer.match(/0/g) || []).length;
        if (zeroCount < fewestZeros) {
            fewestZeros = zeroCount;
            targetLayer = layer;
        }
    }
    return targetLayer;
}

/**
 * Calculates the checksum by multiplying the number of '1's by the number of '2's in a layer.
 * @param {string} layer - The target layer string.
 * @returns {number} - The calculated checksum.
 */
function calculateChecksum(layer) {
    const ones = (layer.match(/1/g) || []).length;
    const twos = (layer.match(/2/g) || []).length;
    return ones * twos;
}

/**
 * Decodes the final image by stacking layers and determining the visible pixels.
 * @param {string[]} layers - Array of layer strings.
 * @param {number} width - Width of the image.
 * @param {number} height - Height of the image.
 * @returns {number[][]} - 2D array representing the final image pixels.
 */
function decodeImage(layers, width, height) {
    const finalImage = Array(width * height).fill(2); // Initialize with transparent pixels

    for (const layer of layers) {
        for (let i = 0; i < layer.length; i++) {
            if (finalImage[i] === 2) { // If pixel is still transparent
                finalImage[i] = parseInt(layer[i], 10);
            }
        }
    }

    // Convert the flat array into a 2D array for better visualization
    const image = [];
    for (let i = 0; i < finalImage.length; i += width) {
        image.push(finalImage.slice(i, i + width));
    }
    return image;
}

/**
 * Displays the decoded image in the console.
 * @param {number[][]} image - 2D array of the final image pixels.
 */
function displayImage(image) {
    const display = image.map(row => 
        row.map(pixel => (pixel === 0 ? ' ' : '█')).join('')
    ).join('\n');
    console.log(display);
}

/**
 * Main function to execute the solution.
 */
function main() {
    const input = readInput('input.txt');
    const width = 25;
    const height = 6;
    const layers = splitIntoLayers(input, width, height);

    // Part 1: Find the layer with the fewest '0's and calculate the checksum
    const layerWithFewestZeros = findLayerWithFewestZeros(layers);
    const checksum = calculateChecksum(layerWithFewestZeros);
    console.log('Part 1: Checksum (Number of 1s * Number of 2s) =', checksum);

    // Part 2: Decode and display the final image
    const finalImage = decodeImage(layers, width, height);
    console.log('Part 2: Decoded Image:');
    displayImage(finalImage);
}

main();
