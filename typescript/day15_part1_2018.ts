
const fs = require('fs');

const KIND_SPACE = 1 << 0;
const KIND_ELF = 1 << 1;
const KIND_GOBLIN = 1 << 2;
const KIND_WALL = 1 << 3;

const RUNE_KINDS: { [key: string]: number } = {
    '.': KIND_SPACE,
    'E': KIND_ELF,
    'G': KIND_GOBLIN,
    '#': KIND_WALL,
};

const OFFSETS = [
    [0, -1],
    [-1, 0],
    [1, 0],
    [0, 1],
];

const DEFAULT_HITPOINTS = 200;
const DEFAULT_POWER = 3;

class Tile {
    kind: number;
    x: number;
    y: number;
    map: Map<number, Map<number, Tile>>;
    unit: Unit | null;

    constructor(kind: number, x: number, y: number, map: Map<number, Map<number, Tile>>) {
        this.kind = kind;
        this.x = x;
        this.y = y;
        this.map = map;
        this.unit = null;
    }

    walkableNeighbors(): Tile[] {
        const neighbors: Tile[] = [];
        for (const offset of OFFSETS) {
            const n = this.map.get(this.y + offset[1])?.get(this.x + offset[0]);
            if (n && n.kind === KIND_SPACE) {
                neighbors.push(n);
            }
        }
        return neighbors;
    }
}

class Unit {
    kind: number;
    hitpoints: number;
    power: number;
    tile: Tile;

    constructor(tile: Tile, kind: number, elfPower: number) {
        this.kind = kind;
        this.hitpoints = DEFAULT_HITPOINTS;
        this.power = kind !== KIND_ELF ? DEFAULT_POWER : elfPower;
        this.tile = tile;
        tile.unit = this;
    }

    targets(c: Cave): boolean {
        return c.units.some(unit => unit.kind !== this.kind && unit.hitpoints > 0);
    }

    nextTile(c: Cave): [Tile | null, Tile | null] {
        let targets: Tile[] = [];
        let closestTargetDistance = Infinity;
        const [distances, path] = findWalkableTiles(c.map, this.tile);
        const enemies = this.enemies(c);

        for (const enemy of enemies) {
            for (const target of enemy.tile.walkableNeighbors()) {
                if (distances.has(target) && distances.get(target)! <= closestTargetDistance) {
                    if (distances.get(target)! < closestTargetDistance) {
                        closestTargetDistance = distances.get(target)!;
                        targets = [];
                    }
                    targets.push(target);
                }
            }
        }

        targets.sort((a, b) => (a.y - b.y) || (a.x - b.x));

        if (targets.length > 0) {
            const target = targets[0];
            let current = target;
            while (path.get(current) !== this.tile) {
                current = path.get(current)!;
            }
            return [current, target];
        }

        return [null, null];
    }

    enemies(c: Cave): Unit[] {
        return [...c.units]
            .filter(unit => unit.kind !== this.kind && unit.hitpoints > 0)
            .sort((a, b) => (a.tile.y - b.tile.y) || (a.tile.x - b.tile.x));
    }

    enemyNeighbor(c: Cave): Unit | null {
        let target: Unit | null = null;
        for (const offset of OFFSETS) {
            const t = c.map.get(this.tile.y + offset[1])?.get(this.tile.x + offset[0]);
            if (t && t.unit && t.unit.kind !== this.kind && t.unit.hitpoints > 0) {
                if (!target || t.unit.hitpoints < target.hitpoints) {
                    target = t.unit;
                }
            }
        }
        return target;
    }

    move(c: Cave) {
        if (this.enemyNeighbor(c)) {
            return;
        }
        const [next, _] = this.nextTile(c);
        if (next) {
            next.unit = this;
            next.kind = this.kind;
            this.tile.kind = KIND_SPACE;
            this.tile.unit = null;
            this.tile = next;
        }
    }

    attack(c: Cave): boolean {
        const enemy = this.enemyNeighbor(c);
        if (enemy) {
            const killed = enemy.damage(c, this.power);
            return killed && enemy.kind === KIND_ELF;
        }
        return false;
    }

    damage(c: Cave, damage: number): boolean {
        this.hitpoints -= damage;
        if (this.hitpoints <= 0) {
            c.removeUnit(this);
            return true;
        }
        return false;
    }
}

class Cave {
    units: Unit[];
    map: Map<number, Map<number, Tile>>;

    constructor(input: string[], elfPower: number) {
        this.units = [];
        this.map = new Map();
        this.parseMap(input, elfPower);
    }

    parseMap(input: string[], elfPower: number) {
        for (let y = 0; y < input.length; y++) {
            for (let x = 0; x < input[y].length; x++) {
                const col = input[y][x];
                const kind = RUNE_KINDS[col] || KIND_WALL;
                const tile = new Tile(kind, x, y, this.map);
                if (isUnit(kind)) {
                    this.units.push(new Unit(tile, kind, elfPower));
                }
                if (!this.map.has(y)) {
                    this.map.set(y, new Map());
                }
                this.map.get(y)!.set(x, tile);
            }
        }
    }

    status(): [number, boolean] {
        let elves = false;
        let goblins = false;
        let hp = 0;
        for (const u of this.units) {
            if (u.hitpoints <= 0) {
                continue;
            }
            if (u.kind === KIND_ELF) {
                elves = true;
            } else {
                goblins = true;
            }
            hp += u.hitpoints;
        }
        return [hp, elves && goblins];
    }

    removeTheDead() {
        this.units = this.units.filter(unit => unit.hitpoints > 0);
    }

    removeUnit(u: Unit) {
        u.tile.kind = KIND_SPACE;
        u.tile.unit = null;
        u.tile = null as any;
    }

    tick(stopOnElfDeath: boolean): [boolean, boolean] {
        this.removeTheDead();
        this.units.sort((a, b) => (a.tile.y - b.tile.y) || (a.tile.x - b.tile.x));
        for (const unit of this.units) {
            if (unit.hitpoints <= 0) {
                continue;
            }
            if (!unit.targets(this)) {
                return [false, false];
            }
            unit.move(this);
            if (unit.attack(this) && stopOnElfDeath) {
                return [false, true];
            }
        }
        return [true, false];
    }
}

function findWalkableTiles(map: Map<number, Map<number, Tile>>, start: Tile): [Map<Tile, number>, Map<Tile, Tile>] {
    const frontier = [start];
    const distance = new Map<Tile, number>();
    const cameFrom = new Map<Tile, Tile>();
    distance.set(start, 0);
    cameFrom.set(start, null as any);

    while (frontier.length > 0) {
        const current = frontier.shift()!;
        for (const next of current.walkableNeighbors()) {
            if (!distance.has(next)) {
                frontier.push(next);
                distance.set(next, distance.get(current)! + 1);
                cameFrom.set(next, current);
            }
        }
    }
    return [distance, cameFrom];
}

function isUnit(bit: number): boolean {
    return (KIND_ELF | KIND_GOBLIN) & bit ? true : false;
}

function combat(input: string[]): number {
    const cave = new Cave(input, DEFAULT_POWER);
    let i = 1;
    while (true) {
        const [hp, combat_] = cave.status();
        if (!combat_) {
            return (i - 1) * hp;
        }
        const [cleanRound, _] = cave.tick(false);
        if (!cleanRound) {
            i -= 1;
        }
        i += 1;
    }
}

const lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map((line: string) => line.trim());
console.log(combat(lines));
