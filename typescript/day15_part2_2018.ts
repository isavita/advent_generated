
import * as fs from 'fs';

const KIND_SPACE = 1;
const KIND_ELF = 2;
const KIND_GOBLIN = 4;
const KIND_WALL = 8;

const RUNE_KINDS: { [key: string]: number } = {
  '.': KIND_SPACE,
  'E': KIND_ELF,
  'G': KIND_GOBLIN,
  '#': KIND_WALL,
};

const isUnit = (bit: number): boolean => (KIND_ELF | KIND_GOBLIN) & bit ? true : false;

const OFFSETS = [
  [0, -1],
  [-1, 0],
  [1, 0],
  [0, 1],
];

const DEFAULT_HITPOINTS = 200;
const DEFAULT_POWER = 3;

type Tile = {
  kind: number;
  x: number;
  y: number;
  unit: Unit | null;
  map: Tile[][];
};

type Unit = {
  kind: number;
  hitpoints: number;
  power: number;
  tile: Tile;
};

type Distances = { [key: string]: number };

function findWalkableTiles(map: Tile[][], start: Tile): [Distances, { [key: string]: Tile }] {
    const frontier: Tile[] = [start];
    const distance: Distances = { [`${start.x},${start.y}`]: 0 };
    const cameFrom: { [key: string]: Tile } = { [`${start.x},${start.y}`]: start };

    while (frontier.length) {
      const current = frontier.shift()!;
      for (const neighbor of walkableNeighbors(current)) {
        if (!(neighbor.x + "," + neighbor.y in distance)) {
          frontier.push(neighbor);
          distance[neighbor.x + "," + neighbor.y] = distance[current.x + "," + current.y] + 1;
          cameFrom[neighbor.x + "," + neighbor.y] = current;
        }
      }
    }
    return [distance, cameFrom];
  }
  

function walkableNeighbors(tile: Tile): Tile[] {
  const neighbors: Tile[] = [];
  for (const offset of OFFSETS) {
    const [dx, dy] = offset;
    const nx = tile.x + dx;
    const ny = tile.y + dy;
     if (tile.map[ny] && tile.map[ny][nx] && tile.map[ny][nx].kind == KIND_SPACE){
        neighbors.push(tile.map[ny][nx]);
    }
  }
  return neighbors;
}

function createCave(input: string[], elfPower: number): [Tile[][], Unit[]] {
    const map: Tile[][] = [];
    const units: Unit[] = [];
  
    for (let y = 0; y < input.length; y++) {
      map[y] = [];
      for (let x = 0; x < input[y].length; x++) {
        const kind = RUNE_KINDS[input[y][x]] || KIND_WALL;
        const tile: Tile = { kind, x, y, unit: null, map };
        
        if (isUnit(kind)) {
          const unit: Unit = {
            kind,
            hitpoints: DEFAULT_HITPOINTS,
            power: kind === KIND_ELF ? elfPower : DEFAULT_POWER,
            tile,
          };
          tile.unit = unit;
          units.push(unit);
        }
        map[y][x]=tile;
      }
    }
  
    return [map, units];
  }
  

function status(units: Unit[]): [number, boolean] {
  let elves = false;
  let goblins = false;
  let hp = 0;

  for (const u of units) {
    if (u.hitpoints <= 0) continue;
    if (u.kind === KIND_ELF) {
      elves = true;
    } else {
      goblins = true;
    }
    hp += u.hitpoints;
  }

  return [hp, elves && goblins];
}

function removeTheDead(units: Unit[]): Unit[] {
   return units.filter((unit) => unit.hitpoints > 0);
}

function removeUnit(unit: Unit): void {
    if (unit.tile) {
      unit.tile.kind = KIND_SPACE;
      unit.tile.unit = null;
      unit.tile = {kind: 0, x: 0, y: 0, unit: null, map: []}
    }
  }

function targets(units: Unit[], unit: Unit): boolean{
    return units.some(u => u.kind !== unit.kind && u.hitpoints > 0);
}
  
function nextTile(map:Tile[][], units:Unit[], unit: Unit): [Tile | null, Tile | null] {
    const targetsList: Tile[] = [];
    let closestTargetDistance = Infinity;
    const [distances, path] = findWalkableTiles(map, unit.tile);
    const enemies = getEnemies(units, unit);
  
    for (const enemy of enemies) {
      for (const target of walkableNeighbors(enemy.tile)) {
        const distKey = `${target.x},${target.y}`;
        if (distKey in distances && distances[distKey] <= closestTargetDistance) {
          if (distances[distKey] < closestTargetDistance) {
            closestTargetDistance = distances[distKey];
            targetsList.length = 0; 
          }
          targetsList.push(target);
        }
      }
    }
  
    targetsList.sort((a, b) => (a.y - b.y) || (a.x - b.x));
  
    if (targetsList.length > 0) {
      const target = targetsList[0];
      let current = target;
      while (path[`${current.x},${current.y}`] !== unit.tile) {
        current = path[`${current.x},${current.y}`];
      }
      return [current, target];
    }
    return [null, null];
  }
  

function getEnemies(units: Unit[], unit: Unit): Unit[] {
  return units
    .filter((u) => u.kind !== unit.kind && u.hitpoints > 0)
    .sort((a, b) => (a.tile.y - b.tile.y) || (a.tile.x - b.tile.x));
}

function enemyNeighbor(map: Tile[][],unit: Unit): Unit | null {
    let target: Unit | null = null;
    for (const offset of OFFSETS) {
      const [dx, dy] = offset;
      const nx = unit.tile.x + dx;
      const ny = unit.tile.y + dy;
      if(map[ny] && map[ny][nx] ) {
      const t = map[ny][nx];

      if (t.unit && t.unit.kind !== unit.kind && t.unit.hitpoints > 0) {
        if (!target || t.unit.hitpoints < target.hitpoints) {
          target = t.unit;
        }
      }
    }
    }
    return target;
  }
  

function move(map: Tile[][], units: Unit[], unit: Unit): void {
  if (enemyNeighbor(map, unit)) {
    return;
  }

  const [next] = nextTile(map, units, unit);
  if (next) {
    next.unit = unit;
    next.kind = unit.kind;
    if(unit.tile){
        unit.tile.kind = KIND_SPACE;
        unit.tile.unit = null;
    }
    unit.tile = next;
  }
}

function attack(map: Tile[][], units: Unit[], unit: Unit): boolean {
  const enemy = enemyNeighbor(map, unit);
  if (enemy) {
    const killed = damage(units, enemy, unit.power);
    return killed && enemy.kind === KIND_ELF;
  }
  return false;
}

function damage(units:Unit[], enemy: Unit, damageValue: number): boolean {
    enemy.hitpoints -= damageValue;
    if (enemy.hitpoints <= 0) {
      removeUnit(enemy);
      return true;
    }
    return false;
  }
  

function tick(map: Tile[][], units: Unit[], stopOnElfDeath: boolean): [boolean, boolean] {
  let currentUnits = removeTheDead(units);
  currentUnits.sort((a, b) => (a.tile.y - b.tile.y) || (a.tile.x - b.tile.x));

    for (const unit of currentUnits)
    {

        if (unit.hitpoints <= 0) {
            continue;
        }
    
    if (!targets(currentUnits, unit)) {
      return [false, false];
    }

        move(map, currentUnits, unit);
    if (attack(map, currentUnits, unit) && stopOnElfDeath) {
      return [false, true];
    }
  }
  return [true, false];
}

function cheatingElves(input: string[]): number {
  let elfDied = true;
  let power = 4;
  let finalHp = 0;
    let finalRounds = 0;


  while (elfDied) {
    const [map, units] = createCave(input, power);
        let i = 1;
        let currentUnits = [...units];
    while (true) {
      const [hp, combat] = status(currentUnits);
          if (!combat) {
                finalHp = hp;
                finalRounds = i -1;
        break;
      }
      const [cleanRound, died] = tick(map, currentUnits, true);
          elfDied = died;
      if (elfDied) {
        break;
      }
      if (!cleanRound) {
        i--;
      }
        i++;
        currentUnits = removeTheDead(currentUnits);

    }
    power++;
  }
    return finalRounds * finalHp;
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
console.log(cheatingElves(input));
