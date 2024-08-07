import * as fs from 'fs';

interface Directory {
    name: string;
    size: number;
    subdirs: Directory[];
}

function parseInput(input: string): Directory {
    const lines = input.split('\n');
    const root: Directory = { name: '/', size: 0, subdirs: [] };
    const stack: Directory[] = [root];
    let currentDir = root;

    for (const line of lines) {
        if (line.startsWith('$ cd')) {
            const dirName = line.split(' ')[2];
            if (dirName === '/') {
                currentDir = root;
            } else if (dirName === '..') {
                stack.pop();
                currentDir = stack[stack.length - 1];
            } else {
                const newDir: Directory = { name: dirName, size: 0, subdirs: [] };
                currentDir.subdirs.push(newDir);
                stack.push(newDir);
                currentDir = newDir;
            }
        } else if (line.startsWith('$ ls')) {
            // No action needed for 'ls' command
        } else if (line.startsWith('dir')) {
            // Directories are created when 'cd' is called
        } else {
            const [sizeStr] = line.split(' ');
            const size = parseInt(sizeStr);
            currentDir.size += size;
        }
    }

    return root;
}

function calculateSizes(dir: Directory): number {
    let totalSize = dir.size;
    for (const subdir of dir.subdirs) {
        totalSize += calculateSizes(subdir);
    }
    dir.size = totalSize; // Update the directory size to include subdirectory sizes
    return totalSize;
}

function sumSmallDirectories(dir: Directory, maxSize: number): number {
    let total = 0;
    if (dir.size <= maxSize) {
        total += dir.size;
    }
    for (const subdir of dir.subdirs) {
        total += sumSmallDirectories(subdir, maxSize);
    }
    return total;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const root = parseInput(input);
    calculateSizes(root);
    const result = sumSmallDirectories(root, 100000);
    console.log(result);
}

main();