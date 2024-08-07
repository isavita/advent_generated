import * as fs from 'fs';

interface AuntSue {
    id: number;
    attributes: Record<string, number>;
}

const mfcsamOutput: Record<string, number> = {
    children: 3,
    cats: 7,
    samoyeds: 2,
    pomeranians: 3,
    akitas: 0,
    vizslas: 0,
    goldfish: 5,
    trees: 3,
    cars: 2,
    perfumes: 1,
};

const parseInput = (data: string): AuntSue[] => {
    return data.split('\n').filter(Boolean).map(line => {
        const parts = line.match(/Sue (\d+): (.+)/);
        if (!parts) return { id: 0, attributes: {} };
        const id = parseInt(parts[1]);
        const attributes = Object.fromEntries(parts[2].split(', ').map(attr => {
            const [key, value] = attr.split(': ');
            return [key, parseInt(value)];
        }));
        return { id, attributes };
    });
};

const findMatchingAunt = (aunts: AuntSue[]): number | null => {
    for (const aunt of aunts) {
        if (Object.entries(aunt.attributes).every(([key, value]) => mfcsamOutput[key] === value)) {
            return aunt.id;
        }
    }
    return null;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const aunts = parseInput(data);
    const matchingAuntId = findMatchingAunt(aunts);
    console.log(matchingAuntId);
};

main();