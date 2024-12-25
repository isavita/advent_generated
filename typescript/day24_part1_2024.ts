
import * as fs from 'fs';

const inputFile = 'input.txt';

const lines = fs.readFileSync(inputFile, 'utf-8').split('\n');

const wires: Map<string, number> = new Map();
const gates: { input1: string; input2: string; operation: string; output: string }[] = [];

const wireRegex = /^(\w+):\s*([01])$/;
const gateRegex = /^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/;

let parsingWires = true;
for (const line of lines) {
  const trimmedLine = line.trim();
  if (trimmedLine === '') {
    parsingWires = false;
    continue;
  }
  if (parsingWires) {
    const matches = wireRegex.exec(trimmedLine);
    if (matches) {
      wires.set(matches[1], parseInt(matches[2]));
    }
  } else {
    const matches = gateRegex.exec(trimmedLine);
    if (matches) {
      gates.push({
        input1: matches[1],
        input2: matches[3],
        operation: matches[2],
        output: matches[4],
      });
    }
  }
}

let remainingGates = gates;
while (remainingGates.length > 0) {
  const newRemainingGates: typeof gates = [];
  for (const gate of remainingGates) {
    const val1 = wires.get(gate.input1);
    const val2 = wires.get(gate.input2);
    if (val1 !== undefined && val2 !== undefined) {
      let outputVal = 0;
      if (gate.operation === 'AND' && val1 === 1 && val2 === 1) {
        outputVal = 1;
      } else if (gate.operation === 'OR' && (val1 === 1 || val2 === 1)) {
        outputVal = 1;
      } else if (gate.operation === 'XOR' && val1 !== val2) {
        outputVal = 1;
      }
      wires.set(gate.output, outputVal);
    } else {
      newRemainingGates.push(gate);
    }
  }
  remainingGates = newRemainingGates;
}

const zWires: Map<number, number> = new Map();
const zRegex = /^z(\d+)$/;
for (const [wire, val] of wires) {
  const matches = zRegex.exec(wire);
  if (matches) {
    zWires.set(parseInt(matches[1]), val);
  }
}

const indices = Array.from(zWires.keys()).sort((a, b) => b - a);
const binaryString = indices.map((idx) => zWires.get(idx)).join('');
const decimalValue = parseInt(binaryString, 2);

console.log(decimalValue);
