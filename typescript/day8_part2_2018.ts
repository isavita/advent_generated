import * as fs from 'fs';

interface Node {
    metadata: number[];
    children: Node[];
}

const parseInput = (input: string): number[] => input.trim().split(' ').map(Number);

const buildTree = (data: number[], index: number): { node: Node; nextIndex: number } => {
    const childCount = data[index++];
    const metadataCount = data[index++];
    const children: Node[] = [];

    for (let i = 0; i < childCount; i++) {
        const childResult = buildTree(data, index);
        children.push(childResult.node);
        index = childResult.nextIndex;
    }

    const metadata = data.slice(index, index + metadataCount);
    index += metadataCount;

    return {
        node: { metadata, children },
        nextIndex: index,
    };
};

const sumMetadata = (node: Node): number => {
    const childMetadataSum = node.children.reduce((sum, child) => sum + sumMetadata(child), 0);
    return childMetadataSum + node.metadata.reduce((sum, value) => sum + value, 0);
};

const calculateNodeValue = (node: Node): number => {
    if (node.children.length === 0) {
        return node.metadata.reduce((sum, value) => sum + value, 0);
    }
    return node.metadata.reduce((sum, index) => {
        if (index === 0 || index > node.children.length) return sum;
        return sum + calculateNodeValue(node.children[index - 1]);
    }, 0);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf8');
    const data = parseInput(input);
    const { node: rootNode } = buildTree(data, 0);

    const metadataSum = sumMetadata(rootNode);
    const rootNodeValue = calculateNodeValue(rootNode);

    console.log(`Sum of all metadata entries: ${metadataSum}`);
    console.log(`Value of the root node: ${rootNodeValue}`);
};

main();