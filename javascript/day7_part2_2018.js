const fs = require('fs');

class Step {
  constructor(id, duration) {
    this.id = id;
    this.duration = duration;
  }
}

function parseInput(filename) {
  const data = fs.readFileSync(filename, 'utf8');
  const lines = data.split('\n');

  const deps = {};
  const allSteps = {};

  for (const line of lines) {
    if (line.trim() === '') continue;
    const [, a, b] = line.match(/Step (\w) must be finished before step (\w) can begin./);
    if (!deps[b]) deps[b] = [];
    deps[b].push(a);
    if (!allSteps[a]) allSteps[a] = new Step(a, a.charCodeAt(0) - 'A'.charCodeAt(0) + 61);
    if (!allSteps[b]) allSteps[b] = new Step(b, b.charCodeAt(0) - 'A'.charCodeAt(0) + 61);
  }

  return [deps, allSteps];
}

function simulateWork(deps, allSteps, numWorkers, baseDuration) {
  const workers = new Array(numWorkers).fill(0);
  const tasks = new Array(numWorkers).fill(null);
  let time = 0;

  while (Object.keys(allSteps).length > 0) {
    const available = Object.keys(allSteps)
      .filter(step => (deps[step] ? deps[step].length === 0 : true) && !tasks.includes(step))
      .sort();

    for (let i = 0; i < numWorkers; i++) {
      if (workers[i] === 0 && available.length > 0) {
        tasks[i] = available.shift();
        workers[i] = allSteps[tasks[i]].duration;
      }
    }

    const minDuration = Math.min(...workers.filter(d => d > 0));
    for (let i = 0; i < numWorkers; i++) {
      if (workers[i] !== 0) {
        workers[i] -= minDuration;
        if (workers[i] === 0) {
          finishStep(deps, allSteps, tasks[i]);
          tasks[i] = null;
        }
      }
    }
    time += minDuration;
  }

  return time;
}

function finishStep(deps, allSteps, step) {
  delete allSteps[step];
  for (const s in deps) {
    deps[s] = deps[s].filter(d => d !== step);
  }
}

// Read input from file and solve the task
const [deps, allSteps] = parseInput('input.txt');
const timeTaken = simulateWork(deps, allSteps, 5, 60);
console.log(timeTaken);