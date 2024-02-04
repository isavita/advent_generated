const fs = require('fs');

function abs(x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

function manhattan(x) {
    return abs(x[0]) + abs(x[1]) + abs(x[2]);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.trim().split('\n');
    const particles = [];

    lines.forEach(line => {
        const parts = line.split(', ');

        const p = { p: [0, 0, 0], v: [0, 0, 0], a: [0, 0, 0] };

        parts.forEach((part, i) => {
            const coords = part.substring(3, part.length - 1).split(',');
            coords.forEach((coord, j) => {
                const num = parseInt(coord);
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
            });
        });

        particles.push(p);
    });

    let closestParticle = 0;
    let minAccel = Number.MAX_SAFE_INTEGER;
    let minVelocity = Number.MAX_SAFE_INTEGER;
    let minPosition = Number.MAX_SAFE_INTEGER;

    particles.forEach((particle, i) => {
        const accel = manhattan(particle.a);
        const velocity = manhattan(particle.v);
        const position = manhattan(particle.p);

        if (accel < minAccel || (accel === minAccel && velocity < minVelocity) ||
            (accel === minAccel && velocity === minVelocity && position < minPosition)) {
            minAccel = accel;
            minVelocity = velocity;
            minPosition = position;
            closestParticle = i;
        }
    });

    console.log(closestParticle);
});