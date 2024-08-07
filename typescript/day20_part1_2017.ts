import * as fs from 'fs';

interface Particle {
    position: number[];
    velocity: number[];
    acceleration: number[];
}

const parseParticle = (line: string): Particle => {
    const regex = /p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>/;
    const match = line.match(regex);
    if (!match) throw new Error(`Invalid particle format: ${line}`);
    
    return {
        position: [parseInt(match[1]), parseInt(match[2]), parseInt(match[3])],
        velocity: [parseInt(match[4]), parseInt(match[5]), parseInt(match[6])],
        acceleration: [parseInt(match[7]), parseInt(match[8]), parseInt(match[9])]
    };
};

const manhattanDistance = (position: number[]): number => 
    Math.abs(position[0]) + Math.abs(position[1]) + Math.abs(position[2]);

const updateParticle = (particle: Particle): void => {
    particle.velocity = particle.velocity.map((v, i) => v + particle.acceleration[i]);
    particle.position = particle.position.map((p, i) => p + particle.velocity[i]);
};

const findClosestParticle = (particles: Particle[]): number => {
    let closestIndex = -1;
    let closestDistance = Infinity;

    particles.forEach((particle, index) => {
        const distance = manhattanDistance(particle.position);
        if (distance < closestDistance) {
            closestDistance = distance;
            closestIndex = index;
        }
    });

    return closestIndex;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const particles = input.map(parseParticle);

    for (let i = 0; i < 1000; i++) {
        particles.forEach(updateParticle);
    }

    const closestParticleIndex = findClosestParticle(particles);
    console.log(`The closest particle index is: ${closestParticleIndex}`);
};

main();