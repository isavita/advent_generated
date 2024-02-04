const fs = require('fs');

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
    let currentMolecule = "";

    for (let i = 0; i < input.length; i++) {
        const code = input.charCodeAt(i);
        if (code >= 65 && code <= 90) {
            molecules.push(input[i]);
        } else {
            currentMolecule += input[i];
            if (i === input.length - 1 || input.charCodeAt(i + 1) >= 65 && input.charCodeAt(i + 1) <= 90) {
                molecules.push(currentMolecule);
                currentMolecule = "";
            }
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

    const start = startingMaterial.join("");
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

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        throw err;
    }
    const input = data.trim();
    console.log(solve(input));
});