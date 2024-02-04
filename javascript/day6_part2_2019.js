const fs = require('fs');

class Node {
    constructor(name) {
        this.name = name;
        this.children = [];
        this.parent = null;
    }
}

function findOrCreateNode(name, nodes) {
    if (name in nodes) {
        return nodes[name];
    }
    const node = new Node(name);
    nodes[name] = node;
    return node;
}

function buildOrbitMap(lines) {
    const nodes = {};
    lines.forEach(line => {
        const parts = line.split(')');
        const center = findOrCreateNode(parts[0], nodes);
        const orbiter = findOrCreateNode(parts[1], nodes);
        center.children.push(orbiter);
        orbiter.parent = center;
    });
    return nodes;
}

function pathToRoot(node) {
    const path = [];
    while (node !== null) {
        path.push(node);
        node = node.parent;
    }
    return path;
}

function findCommonAncestor(node1, node2) {
    const path1 = pathToRoot(node1);
    const path2 = pathToRoot(node2);

    let i = path1.length - 1;
    let j = path2.length - 1;

    while (i >= 0 && j >= 0 && path1[i] === path2[j]) {
        i--;
        j--;
    }
    return i + 1 + j + 1;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading file:', err);
        return;
    }

    const lines = data.trim().split('\n');
    const orbitMap = buildOrbitMap(lines);

    const transfers = findCommonAncestor(orbitMap['YOU'].parent, orbitMap['SAN'].parent);
    console.log(transfers);
});