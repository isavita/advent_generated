import * as fs from 'fs';

const WIDTH = 25;
const HEIGHT = 6;
const LAYER_SIZE = WIDTH * HEIGHT;

function main() {
    const data = fs.readFileSync('input.txt', 'utf-8').trim();
    const layers = splitIntoLayers(data, LAYER_SIZE);
    const layerWithFewestZeros = findLayerWithFewestZeros(layers);
    const result = countDigits(layerWithFewestZeros);
    console.log(result[1] * result[2]); // Multiply 1s and 2s
}

function splitIntoLayers(data: string, layerSize: number): string[] {
    const layers: string[] = [];
    for (let i = 0; i < data.length; i += layerSize) {
        layers.push(data.slice(i, i + layerSize));
    }
    return layers;
}

function findLayerWithFewestZeros(layers: string[]): string {
    return layers.reduce((prev, curr) => {
        return countDigits(prev)[0] <= countDigits(curr)[0] ? prev : curr;
    });
}

function countDigits(layer: string): [number, number, number] {
    const counts: [number, number, number] = [0, 0, 0];
    for (const char of layer) {
        const digit = parseInt(char);
        if (digit >= 0 && digit <= 2) {
            counts[digit]++;
        }
    }
    return counts;
}

main();