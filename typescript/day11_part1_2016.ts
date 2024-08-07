import * as fs from 'fs';
import * as readline from 'readline';

interface Item {
    element: string;
    type: 'G' | 'M';
}

interface State {
    elevator: number;
    floors: Item[][];
}

function isValid(state: State): boolean {
    for (let floor of state.floors) {
        let generators = floor.filter(item => item.type === 'G').map(item => item.element);
        let microchips = floor.filter(item => item.type === 'M').map(item => item.element);

        for (let chip of microchips) {
            if (!generators.includes(chip) && generators.length > 0) {
                return false;
            }
        }
    }
    return true;
}

function isFinal(state: State): boolean {
    return state.floors[0].length === 0 && state.floors[1].length === 0 && state.floors[2].length === 0;
}

function generateNextStates(state: State): State[] {
    let nextStates: State[] = [];
    let currentFloor = state.floors[state.elevator];

    for (let i = 0; i < currentFloor.length; i++) {
        for (let j = i; j < currentFloor.length; j++) {
            if (state.elevator < 3) {
                let newState = JSON.parse(JSON.stringify(state)) as State;
                newState.elevator += 1;
                newState.floors[state.elevator].push(currentFloor[i]);
                if (i !== j) {
                    newState.floors[state.elevator].push(currentFloor[j]);
                }
                newState.floors[state.elevator - 1] = newState.floors[state.elevator - 1].filter(item => item !== currentFloor[i] && item !== currentFloor[j]);
                if (isValid(newState)) {
                    nextStates.push(newState);
                }
            }
            if (state.elevator > 0) {
                let newState = JSON.parse(JSON.stringify(state)) as State;
                newState.elevator -= 1;
                newState.floors[state.elevator].push(currentFloor[i]);
                if (i !== j) {
                    newState.floors[state.elevator].push(currentFloor[j]);
                }
                newState.floors[state.elevator + 1] = newState.floors[state.elevator + 1].filter(item => item !== currentFloor[i] && item !== currentFloor[j]);
                if (isValid(newState)) {
                    nextStates.push(newState);
                }
            }
        }
    }

    return nextStates;
}

function stateToString(state: State): string {
    return `${state.elevator}-${state.floors.map(floor => floor.map(item => item.element + item.type).sort().join('')).join('|')}`;
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let floors: Item[][] = [[], [], [], []];
    let currentFloor = 0;

    for await (const line of rl) {
        let matches = line.match(/(\w+)( generator|-compatible microchip)/g);
        if (matches) {
            matches.forEach(match => {
                let [element, type] = match.split(' ');
                floors[currentFloor].push({ element, type: type[0] as 'G' | 'M' });
            });
        } else if (line.includes('The')) {
            currentFloor++;
        }
    }

    let initialState: State = {
        elevator: 0,
        floors: floors
    };

    let queue: [State, number][] = [[initialState, 0]];
    let visited = new Set<string>();
    visited.add(stateToString(initialState));

    while (queue.length > 0) {
        let [currentState, steps] = queue.shift()!;

        if (isFinal(currentState)) {
            console.log(steps);
            return;
        }

        for (let nextState of generateNextStates(currentState)) {
            let nextStateStr = stateToString(nextState);
            if (!visited.has(nextStateStr)) {
                visited.add(nextStateStr);
                queue.push([nextState, steps + 1]);
            }
        }
    }
}

main().catch(console.error);