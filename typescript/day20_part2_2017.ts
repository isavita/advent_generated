import * as fs from 'fs';

interface Particle {
    position: number[];
    velocity: number[];
    acceleration: number[];
}

function parseInput(filePath: string): Particle[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').map(line => {
        const regex = /p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>/;
        const [, p, v, a] = line.match(regex)!;
        return {
            position: p.split(',').map(Number),
            velocity: v.split(',').map(Number),
            acceleration: a.split(',').map(Number),
        };
    });
}

function updateParticle(particle: Particle): void {
    for (let i = 0; i < 3; i++) {
        particle.velocity[i] += particle.acceleration[i];
        particle.position[i] += particle.velocity[i];
    }
}

function manhattanDistance(position: number[]): number {
    return position.reduce((sum, coord) => sum + Math.abs(coord), 0);
}

function findClosestParticle(particles: Particle[]): number {
    return particles.reduce((closestIndex, particle, index) => {
        const distance = manhattanDistance(particle.position);
        return distance < manhattanDistance(particles[closestIndex].position) ? index : closestIndex;
    }, 0);
}

function resolveCollisions(particles: Particle[]): number {
    let particlesLeft = particles.slice();
    let collisions: Set<number> = new Set();

    for (let tick = 0; tick < 100; tick++) {
        const positions = new Map<string, number[]>();

        particlesLeft.forEach((particle, index) => {
            updateParticle(particle);
            const posKey = particle.position.join(',');
            if (!positions.has(posKey)) {
                positions.set(posKey, []);
            }
            positions.get(posKey)!.push(index);
        });

        collisions = new Set();
        positions.forEach(indices => {
            if (indices.length > 1) {
                indices.forEach(i => collisions.add(i));
            }
        });

        particlesLeft = particlesLeft.filter((_, index) => !collisions.has(index));
    }

    return particlesLeft.length;
}

const particles = parseInput('input.txt');
const closestParticleIndex = findClosestParticle(particles);
console.log(`Closest particle index: ${closestParticleIndex}`);
const remainingParticles = resolveCollisions(particles);
console.log(`Remaining particles after collisions: ${remainingParticles}`);