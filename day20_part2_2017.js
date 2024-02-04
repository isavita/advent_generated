const fs = require('fs');

class Particle {
    constructor() {
        this.p = [0, 0, 0];
        this.v = [0, 0, 0];
        this.a = [0, 0, 0];
    }
}

const particles = [];
const input = fs.readFileSync('input.txt', 'utf8').split('\n');

for (let line of input) {
    const parts = line.split(', ');

    const p = new Particle();
    for (let i = 0; i < parts.length; i++) {
        const coords = parts[i].substring(3, parts[i].length - 1).split(',');
        for (let j = 0; j < coords.length; j++) {
            const num = parseInt(coords[j]);
            switch (i) {
                case 0:
                    p.p[j] = num;
                    break;
                case 1:
                    p.v[j] = num;
                    break;
                case 2:
                    p.a[j] = num;
                    break;
            }
        }
    }
    particles.push(p);
}

for (let tick = 0; tick < 1000; tick++) {
    const positions = {};

    for (let i = 0; i < particles.length; i++) {
        const particle = particles[i];
        for (let j = 0; j < 3; j++) {
            particle.v[j] += particle.a[j];
            particle.p[j] += particle.v[j];
        }
        particles[i] = particle;

        const posStr = `${particle.p[0]},${particle.p[1]},${particle.p[2]}`;
        positions[posStr] = (positions[posStr] || 0) + 1;
    }

    const newParticles = particles.filter(particle => {
        const posStr = `${particle.p[0]},${particle.p[1]},${particle.p[2]}`;
        return positions[posStr] === 1;
    });

    particles.length = 0;
    particles.push(...newParticles);
}

console.log(particles.length);