import * as fs from 'fs';

// Function to read input from file
function readInput(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8');
}

// Function to parse the input into replacement rules and the medicine molecule
function parseInput(input: string): { replacements: Map<string, string[]>, molecule: string } {
    const lines = input.split('\n').filter(line => line.trim() !== '');
    const replacements = new Map<string, string[]>();
    let molecule = '';

    lines.forEach(line => {
        if (line.includes('=>')) {
            const [from, to] = line.split(' => ');
            if (!replacements.has(from)) {
                replacements.set(from, []);
            }
            replacements.get(from)!.push(to);
        } else {
            molecule = line;
        }
    });

    return { replacements, molecule };
}

// Function to generate distinct molecules after one replacement
function generateDistinctMolecules(replacements: Map<string, string[]>, molecule: string): Set<string> {
    const distinctMolecules = new Set<string>();

    for (let [from, tos] of replacements) {
        for (let to of tos) {
            let index = 0;
            while ((index = molecule.indexOf(from, index)) !== -1) {
                const newMolecule = molecule.slice(0, index) + to + molecule.slice(index + from.length);
                distinctMolecules.add(newMolecule);
                index += from.length;
            }
        }
    }

    return distinctMolecules;
}

// Main function to execute the program
function main() {
    const input = readInput('input.txt');
    const { replacements, molecule } = parseInput(input);
    const distinctMolecules = generateDistinctMolecules(replacements, molecule);
    console.log(distinctMolecules.size);
}

// Execute the main function
main();