const fs = require('fs');

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    console.log(solve(input));
}

function parseInput(input) {
    const blocks = input.split("\n\n");
    const startingMaterial = splitMolecules(blocks[1]);

    const graph = {};

    blocks[0].split("\n").forEach(l => {
        const parts = l.split(" => ");
        if (!graph[parts[0]]) {
            graph[parts[0]] = [];
        }
        graph[parts[0]].push(parts[1]);
    });

    return { graph, startingMaterial };
}

function splitMolecules(input) {
    const molecules = [];
    let i = 0;
    while (i < input.length) {
        if (input[i] >= 'A' && input[i] <= 'Z') {
            molecules.push(input[i]);
            i++;
        } else {
            molecules[molecules.length - 1] += input[i];
            i++;
        }
    }
    return molecules;
}

function solve(input) {
    const { graph, startingMaterial } = parseInput(input);

    const productToReactant = {};
    for (const react in graph) {
        graph[react].forEach(prod => {
            if (productToReactant[prod]) {
                throw new Error("dup found");
            }
            productToReactant[prod] = react;
        });
    }

    const allProducts = Object.keys(productToReactant);

    let start = startingMaterial.join("");
    let mol = start;

    let steps = 0;
    while (mol !== "e") {
        let changeMade = false;
        for (const prod of allProducts) {
            const count = mol.split(prod).length - 1;
            if (count <= 0) {
                continue;
            }
            changeMade = true;
            steps += count;
            mol = mol.replace(new RegExp(prod, 'g'), productToReactant[prod]);
            break;
        }

        if (!changeMade) {
            allProducts.sort(() => Math.random() - 0.5);
            mol = start;
            steps = 0;
        }
    }

    return steps;
}

main();