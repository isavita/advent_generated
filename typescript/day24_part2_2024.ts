
import * as fs from 'fs';

interface Gate {
    a: string;
    op: string;
    b: string;
}

interface GateWithOutput {
    gate: Gate;
    output: string;
}

function parse(input: string): GateWithOutput[] | null {
    const parts = input.split("\n\n");
    if (parts.length !== 2) {
        return null;
    }

    const gates: GateWithOutput[] = [];
    for (const line of parts[1].split("\n")) {
        if (!line) continue;
        const parts = line.split(" -> ");
        if (parts.length !== 2) continue;
        const gateParts = parts[0].split(" ");
        if (gateParts.length !== 3) continue;
        gates.push({
            gate: { a: gateParts[0], op: gateParts[1], b: gateParts[2] },
            output: parts[1],
        });
    }
    return gates;
}

function createLookups(gates: GateWithOutput[]): [Map<string, Gate>, Map<string, string>] {
    const lookup = new Map<string, Gate>();
    const reverseLookup = new Map<string, string>();

    for (const g of gates) {
        lookup.set(g.output, g.gate);
        const inputs = [g.gate.a, g.gate.b].sort();
        const key = `${inputs[0]}_${g.gate.op}_${inputs[1]}`;
        reverseLookup.set(key, g.output);
    }
    return [lookup, reverseLookup];
}

function swap(pairs: [string, string][], gates: GateWithOutput[], a: string, b: string): void {
    pairs.push([a, b]);
    for (let i = 0; i < gates.length; i++) {
        if (gates[i].output === a) {
            gates[i].output = b;
        } else if (gates[i].output === b) {
            gates[i].output = a;
        }
    }
}

function getReverseLookupKey(a: string, op: string, b: string): string {
    const inputs = [a, b].sort();
    return `${inputs[0]}_${op}_${inputs[1]}`;
}

function solution(gates: GateWithOutput[]): string {
    const pairs: [string, string][] = [];
    let numZ = 0;
    for (const g of gates) {
        if (g.output.startsWith("z")) {
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
                adder = reverseLookup.get(getReverseLookupKey(xi, "XOR", yi)) || "";
                carry = reverseLookup.get(getReverseLookupKey(xi, "AND", yi)) || "";
            } else {
                const bit = reverseLookup.get(getReverseLookupKey(xi, "XOR", yi)) || "";
                if (bit) {
                    adder = reverseLookup.get(getReverseLookupKey(bit, "XOR", carry)) || "";
                    if (adder) {
                        const c1 = reverseLookup.get(getReverseLookupKey(xi, "AND", yi)) || "";
                        const c2 = reverseLookup.get(getReverseLookupKey(bit, "AND", carry)) || "";
                        carry = reverseLookup.get(getReverseLookupKey(c1, "OR", c2)) || "";
                    }
                }
            }

            if (!adder) {
                const gate = lookup.get(zi);
                if(gate){
                    const bitKey = getReverseLookupKey(xi, "XOR", yi);
                    const bit = reverseLookup.get(bitKey) || "";
                    if (reverseLookup.get(getReverseLookupKey(gate.a, "XOR", carry))) {
                        swap(pairs, gates, bit, gate.a);
                        break;
                    } else if (reverseLookup.get(getReverseLookupKey(gate.b, "XOR", carry))) {
                        swap(pairs, gates, bit, gate.b);
                        break;
                    }
                }
            } else if (adder !== zi) {
                swap(pairs, gates, adder, zi);
                break;
            }
        }
    }

    const result = pairs.flat().sort();
    return result.join(",");
}

try {
    const input = fs.readFileSync("input.txt", "utf-8");
    const gates = parse(input);
    if (!gates) {
        console.error("Error parsing input");
    } else {
        console.log(solution(gates));
    }
} catch (err) {
    console.error("Error reading input file:", err);
}
