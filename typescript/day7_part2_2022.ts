import * as fs from 'fs';
import * as path from 'path';

interface Node {
    name: string;
    size: number; // size of files
    children: Node[];
}

const parseInput = (input: string): Node => {
    const root: Node = { name: '/', size: 0, children: [] };
    const stack: Node[] = [root];
    let currentDir = root;

    input.split('\n').forEach(line => {
        if (line.startsWith('$')) {
            const command = line.split(' ').slice(1);
            if (command[0] === 'cd') {
                if (command[1] === '..') {
                    stack.pop();
                    currentDir = stack[stack.length - 1];
                } else if (command[1] === '/') {
                    currentDir = root;
                    stack.length = 1; // reset stack
                } else {
                    const newDir = currentDir.children.find(child => child.name === command[1]);
                    if (newDir) {
                        stack.push(newDir);
                        currentDir = newDir;
                    }
                }
            }
        } else if (line.startsWith('dir')) {
            const dirName = line.split(' ')[1];
            const newDir: Node = { name: dirName, size: 0, children: [] };
            currentDir.children.push(newDir);
        } else {
            const [sizeStr, fileName] = line.split(' ');
            const size = parseInt(sizeStr);
            currentDir.children.push({ name: fileName, size, children: [] });
            currentDir.size += size;
        }
    });

    return root;
};

const calculateSizes = (node: Node): number => {
    let totalSize = node.size;
    for (const child of node.children) {
        if (child.children.length > 0) {
            totalSize += calculateSizes(child);
        }
    }
    node.size = totalSize;
    return totalSize;
};

const findSmallDirectories = (node: Node, limit: number, smallDirs: number[]): void => {
    if (node.size <= limit) {
        smallDirs.push(node.size);
    }
    for (const child of node.children) {
        if (child.children.length > 0) {
            findSmallDirectories(child, limit, smallDirs);
        }
    }
};

const findSmallestDirToDelete = (node: Node, neededSpace: number, minSize: number): number => {
    let result = minSize;
    if (node.size >= neededSpace && node.size < result) {
        result = node.size;
    }
    for (const child of node.children) {
        if (child.children.length > 0) {
            result = Math.min(result, findSmallestDirToDelete(child, neededSpace, result));
        }
    }
    return result;
};

const main = () => {
    const input = fs.readFileSync(path.join(__dirname, 'input.txt'), 'utf-8');
    const root = parseInput(input);
    calculateSizes(root);

    const smallDirs: number[] = [];
    findSmallDirectories(root, 100000, smallDirs);
    const partOneResult = smallDirs.reduce((sum, size) => sum + size, 0);
    console.log(`Part One: ${partOneResult}`);

    const totalSpace = 70000000;
    const requiredSpace = 30000000;
    const usedSpace = root.size;
    const neededSpace = requiredSpace - (totalSpace - usedSpace);
    const partTwoResult = findSmallestDirToDelete(root, neededSpace, Infinity);
    console.log(`Part Two: ${partTwoResult}`);
};

main();