import * as fs from 'fs';

interface Tile {
    id: number;
    data: string[];
}

const readTiles = (filename: string): Tile[] => {
    const content = fs.readFileSync(filename, 'utf-8').trim().split('\n\n');
    return content.map(block => {
        const lines = block.split('\n');
        const id = parseInt(lines[0].match(/Tile (\d+):/)![1]);
        const data = lines.slice(1);
        return { id, data };
    });
};

const rotate = (tile: string[]): string[] => {
    const n = tile.length;
    return Array.from({ length: n }, (_, i) => tile.map(row => row[i]).reverse().join(''));
};

const flip = (tile: string[]): string[] => {
    return tile.map(row => row.split('').reverse().join(''));
};

const getEdges = (tile: string[]): string[] => {
    const top = tile[0];
    const bottom = tile[tile.length - 1];
    const left = tile.map(row => row[0]).join('');
    const right = tile.map(row => row[row.length - 1]).join('');
    return [top, right, bottom, left];
};

const findCorners = (tiles: Tile[]): number => {
    const edgesMap = new Map<string, number[]>();
    
    tiles.forEach(tile => {
        const edges = getEdges(tile.data);
        edges.forEach(edge => {
            if (!edgesMap.has(edge)) edgesMap.set(edge, []);
            edgesMap.get(edge)!.push(tile.id);
            const reversedKey = edge.split('').reverse().join('');
            if (!edgesMap.has(reversedKey)) edgesMap.set(reversedKey, []);
            edgesMap.get(reversedKey)!.push(tile.id);
        });
    });

    const cornerIds: Set<number> = new Set();
    
    tiles.forEach(tile => {
        const edges = getEdges(tile.data);
        const count = edges.reduce((acc, edge) => acc + (edgesMap.get(edge)?.length || 0), 0);
        if (count === 6) cornerIds.add(tile.id);
    });

    return Array.from(cornerIds).reduce((product, id) => product * id, 1);
};

const main = () => {
    const tiles = readTiles('input.txt');
    const result = findCorners(tiles);
    console.log(result);
};

main();