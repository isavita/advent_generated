
const fs = require('fs');

try {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const lines = input.split('\n');

    const wires = {};
    const gates = [];
    let parsingWires = true;

    const wireRegex = /^(\w+):\s*([01])$/;
    const gateRegex = /^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/;

    for (const line of lines) {
        const trimmedLine = line.trim();
        if (trimmedLine === '') {
            parsingWires = false;
            continue;
        }

        if (parsingWires) {
            const wireMatch = trimmedLine.match(wireRegex);
            if (wireMatch) {
                wires[wireMatch[1]] = parseInt(wireMatch[2], 10);
            }
        } else {
            const gateMatch = trimmedLine.match(gateRegex);
            if (gateMatch) {
                gates.push({
                    input1: gateMatch[1],
                    operation: gateMatch[2],
                    input2: gateMatch[3],
                    output: gateMatch[4],
                });
            }
        }
    }

    let remainingGates = [...gates];
    while (remainingGates.length > 0) {
        let progress = false;
        const newRemainingGates = [];
        for (const gate of remainingGates) {
            const val1 = wires[gate.input1];
            const val2 = wires[gate.input2];
            if (val1 !== undefined && val2 !== undefined) {
                let outputVal;
                switch (gate.operation) {
                    case 'AND':
                        outputVal = (val1 === 1 && val2 === 1) ? 1 : 0;
                        break;
                    case 'OR':
                        outputVal = (val1 === 1 || val2 === 1) ? 1 : 0;
                        break;
                    case 'XOR':
                        outputVal = (val1 !== val2) ? 1 : 0;
                        break;
                }
                wires[gate.output] = outputVal;
                progress = true;
            } else {
                newRemainingGates.push(gate);
            }
        }
        if (!progress) {
            console.log("Cannot evaluate remaining gates due to missing inputs or cyclic dependencies.");
            process.exit(1);
        }
        remainingGates = newRemainingGates;
    }

    const zWires = {};
    const zRegex = /^z(\d+)$/;
    for (const wire in wires) {
        const zMatch = wire.match(zRegex);
        if (zMatch) {
            zWires[parseInt(zMatch[1], 10)] = wires[wire];
        }
    }

    if (Object.keys(zWires).length === 0) {
        console.log("No wires starting with 'z' found.");
        process.exit(1);
    }

    const sortedIndices = Object.keys(zWires).map(Number).sort((a, b) => b - a);
    let binaryString = '';
    for (const idx of sortedIndices) {
        binaryString += zWires[idx];
    }

    const decimalValue = parseInt(binaryString, 2);
    console.log(decimalValue);

} catch (err) {
    console.error("Error:", err);
}
