const fs = require('fs');

// Read and parse the input instructions
const instructions = fs.readFileSync('input.txt', 'utf-8')
  .trim()
  .split('\n')
  .map(line => line.split(' '));

// Helper function to get the value (either register or integer)
const getValue = (reg, x) => {
  if (isNaN(x)) {
    return reg[x] || 0;
  }
  return parseInt(x, 10);
};

// Program class to represent each of the two programs
class Program {
  constructor(id, instructions, sendQueue, receiveQueue) {
    this.id = id;
    this.instructions = instructions;
    this.registers = { p: id };
    this.sendQueue = sendQueue;
    this.receiveQueue = receiveQueue;
    this.pointer = 0;
    this.sendCount = 0;
    this.waiting = false;
    this.terminated = false;
  }

  step() {
    if (this.pointer < 0 || this.pointer >= this.instructions.length) {
      this.terminated = true;
      return;
    }

    const [op, x, y] = this.instructions[this.pointer];

    switch (op) {
      case 'snd':
        const value = getValue(this.registers, x);
        this.sendQueue.push(value);
        this.sendCount += 1;
        this.pointer += 1;
        break;

      case 'set':
        this.registers[x] = getValue(this.registers, y);
        this.pointer += 1;
        break;

      case 'add':
        this.registers[x] = (getValue(this.registers, x) || 0) + getValue(this.registers, y);
        this.pointer += 1;
        break;

      case 'mul':
        this.registers[x] = (getValue(this.registers, x) || 0) * getValue(this.registers, y);
        this.pointer += 1;
        break;

      case 'mod':
        this.registers[x] = (getValue(this.registers, x) || 0) % getValue(this.registers, y);
        this.pointer += 1;
        break;

      case 'rcv':
        if (this.receiveQueue.length > 0) {
          this.registers[x] = this.receiveQueue.shift();
          this.waiting = false;
          this.pointer += 1;
        } else {
          this.waiting = true;
        }
        break;

      case 'jgz':
        if (getValue(this.registers, x) > 0) {
          this.pointer += getValue(this.registers, y);
        } else {
          this.pointer += 1;
        }
        break;

      default:
        console.error(`Unknown operation: ${op}`);
        this.pointer += 1;
    }
  }
}

// Initialize message queues for both programs
const queue0 = [];
const queue1 = [];

// Create two program instances
const program0 = new Program(0, instructions, queue0, queue1);
const program1 = new Program(1, instructions, queue1, queue0);

// Keep track of whether the programs are deadlocked
while (true) {
  const prevWaiting0 = program0.waiting;
  const prevWaiting1 = program1.waiting;

  // Execute a step for each program
  if (!program0.terminated) {
    program0.step();
  }

  if (!program1.terminated) {
    program1.step();
  }

  // Check for deadlock: both programs are waiting
  if (
    (program0.waiting || program0.terminated) &&
    (program1.waiting || program1.terminated)
  ) {
    break;
  }

  // If both programs have terminated, exit
  if (program0.terminated && program1.terminated) {
    break;
  }
}

// Output the number of times program 1 sent a value
console.log(`Program 1 sent a value ${program1.sendCount} times.`);
