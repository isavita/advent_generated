
const fs = require('node:fs');

function parse(input) {
    const parts = input.split('\n\n');
    if (parts.length !== 2) return null;

    const gates = [];
    for (const line of parts[1].split('\n')) {
        if (!line) continue;
        const [gatePart, output] = line.split(' -> ');
        if (!output) continue;
        const gateParts = gatePart.split(' ');
        if (gateParts.length !== 3) continue;
        gates.push({
            gate: { a: gateParts[0], op: gateParts[1], b: gateParts[2] },
            output,
        });
    }
    return gates;
}

function createLookups(gates) {
    const lookup = {};
    const reverseLookup = {};

    for (const g of gates) {
        lookup[g.output] = g.gate;
        const inputs = [g.gate.a, g.gate.b].sort();
        const key = `${inputs[0]}_${g.gate.op}_${inputs[1]}`;
        reverseLookup[key] = g.output;
    }
    return [lookup, reverseLookup];
}

function swap(pairs, gates, a, b) {
    pairs.push([a, b]);
    for (let i = 0; i < gates.length; i++) {
        if (gates[i].output === a) {
            gates[i].output = b;
        } else if (gates[i].output === b) {
            gates[i].output = a;
        }
    }
}

function getReverseLookupKey(a, op, b) {
    const inputs = [a, b].sort();
    return `${inputs[0]}_${op}_${inputs[1]}`;
}

function solution(gates) {
    const pairs = [];
    let numZ = 0;
    for (const g of gates) {
        if (g.output.startsWith('z')) {
            numZ++;
        }
    }

    while (pairs.length < 4) {
        let adder = "";
        let carry = "";
        const [lookup, reverseLookup] = createLookups(gates);

        for (let i = 0; i < numZ; i++) {
            const xi = `x${String(i).padStart(2, '0')}`;
            const yi = `y${String(i).padStart(2, '0')}`;
            const zi = `z${String(i).padStart(2, '0')}`;

            if (i === 0) {
                adder = reverseLookup[getReverseLookupKey(xi, "XOR", yi)];
                carry = reverseLookup[getReverseLookupKey(xi, "AND", yi)];
            } else {
                const bit = reverseLookup[getReverseLookupKey(xi, "XOR", yi)];
                if (bit) {
                    adder = reverseLookup[getReverseLookupKey(bit, "XOR", carry)];
                    if (adder) {
                        const c1 = reverseLookup[getReverseLookupKey(xi, "AND", yi)];
                        const c2 = reverseLookup[getReverseLookupKey(bit, "AND", carry)];
                        carry = reverseLookup[getReverseLookupKey(c1, "OR", c2)];
                    }
                }
            }

            if (!adder) {
                const gate = lookup[zi];
                const bitKey = getReverseLookupKey(xi, "XOR", yi);
                const bit = reverseLookup[bitKey];
                if (reverseLookup[getReverseLookupKey(gate.a, "XOR", carry)]) {
                    swap(pairs, gates, bit, gate.a);
                    break;
                } else if (reverseLookup[getReverseLookupKey(gate.b, "XOR", carry)]) {
                    swap(pairs, gates, bit, gate.b);
                    break;
                }
            } else if (adder !== zi) {
                swap(pairs, gates, adder, zi);
                break;
            }
        }
    }

    const result = pairs.flat().sort();
    return result.join(',');
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading input file:', err);
        return;
    }
    const gates = parse(data);
    if (!gates) {
        console.error('Error parsing input');
        return;
    }
    console.log(solution(gates));
});
