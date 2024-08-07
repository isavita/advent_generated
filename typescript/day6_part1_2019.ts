import * as fs from 'fs';

const calculateOrbits = (input: string): number => {
    const orbits = input.trim().split('\n').map(line => line.split(')'));
    const orbitMap = new Map<string, string>();

    orbits.forEach(([center, orbiter]) => {
        orbitMap.set(orbiter, center);
    });

    const countOrbits = (object: string): number => {
        let count = 0;
        while (orbitMap.has(object)) {
            object = orbitMap.get(object)!;
            count++;
        }
        return count;
    };

    return Array.from(orbitMap.keys()).reduce((total, orbiter) => total + countOrbits(orbiter), 0);
};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const totalOrbits = calculateOrbits(data);
    console.log(totalOrbits);
});