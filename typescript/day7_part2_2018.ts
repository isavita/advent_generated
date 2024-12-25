
interface Step {
  id: string;
  duration: number;
}

function parseInput(filename: string): [Map<string, string[]>, Map<string, Step>] {
  const fs = require('fs');
  const lines = fs.readFileSync(filename, 'utf-8').split('\n');
  const deps = new Map<string, string[]>();
  const allSteps = new Map<string, Step>();

  for (const line of lines) {
    if (line === '') continue;
    const matches = line.match(/Step (\w) must be finished before step (\w) can begin./);
    if (matches) {
      const [, a, b] = matches;
      if (!deps.has(b)) {
        deps.set(b, []);
      }
      deps.get(b)!.push(a);
      if (!allSteps.has(a)) {
        allSteps.set(a, { id: a, duration: a.charCodeAt(0) - 'A'.charCodeAt(0) + 61 });
      }
      if (!allSteps.has(b)) {
        allSteps.set(b, { id: b, duration: b.charCodeAt(0) - 'A'.charCodeAt(0) + 61 });
      }
    }
  }
  return [deps, allSteps];
}

function simulateWork(deps: Map<string, string[]>, allSteps: Map<string, Step>, numWorkers: number, baseDuration: number): number {
  const workers: number[] = new Array(numWorkers).fill(0);
  const tasks: string[] = new Array(numWorkers).fill('');
  let time = 0;

  while (allSteps.size > 0) {
    const available: string[] = [];
    for (const [step, _] of allSteps) {
      if ((!deps.has(step) || deps.get(step)!.length === 0) && !tasks.includes(step)) {
        available.push(step);
      }
    }
    available.sort();

    for (let i = 0; i < numWorkers; i++) {
      if (workers[i] === 0 && available.length > 0) {
        tasks[i] = available[0];
        workers[i] = allSteps.get(available[0])!.duration;
        available.shift();
      }
    }

    let minDuration = Infinity;
    for (const duration of workers) {
      if (duration > 0 && duration < minDuration) {
        minDuration = duration;
      }
    }

    for (let i = 0; i < numWorkers; i++) {
      if (workers[i] !== 0) {
        workers[i] -= minDuration;
        if (workers[i] === 0) {
          finishStep(deps, allSteps, tasks[i]);
          tasks[i] = '';
        }
      }
    }
    time += minDuration;
  }

  return time;
}

function finishStep(deps: Map<string, string[]>, allSteps: Map<string, Step>, step: string) {
  allSteps.delete(step);
  for (const [s, ] of allSteps) {
      if (deps.has(s)) {
          const index = deps.get(s)!.indexOf(step);
          if (index > -1) {
              deps.get(s)!.splice(index, 1);
          }
      }
  }
}

const [deps, allSteps] = parseInput('input.txt');
const timeTaken = simulateWork(deps, allSteps, 5, 60);
console.log(timeTaken);
