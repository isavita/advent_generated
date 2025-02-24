
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class CombatSimulator {

    private static final int KIND_SPACE = 1;
    private static final int KIND_ELF = 2;
    private static final int KIND_GOBLIN = 4;
    private static final int KIND_WALL = 8;

    private static final int DEFAULT_HITPOINTS = 200;
    private static final int DEFAULT_POWER = 3;

    private static final int[][] OFFSETS = {{0, -1}, {-1, 0}, {1, 0}, {0, 1}};

    static class Cave {
        List<Unit> units = new ArrayList<>();
        Map<Integer, Map<Integer, Tile>> map = new HashMap<>();

        Cave(List<String> input, int elfPower) {
            parseMap(input, elfPower);
        }

        void parseMap(List<String> input, int elfPower) {
            for (int y = 0; y < input.size(); y++) {
                String row = input.get(y);
                for (int x = 0; x < row.length(); x++) {
                    char col = row.charAt(x);
                    int kind = switch (col) {
                        case '.' -> KIND_SPACE;
                        case 'E' -> KIND_ELF;
                        case 'G' -> KIND_GOBLIN;
                        case '#' -> KIND_WALL;
                        default -> KIND_WALL;
                    };
                    Tile tile = new Tile(kind, x, y, map);
                    map.computeIfAbsent(y, k -> new HashMap<>()).put(x, tile);
                    if (isUnit(kind)) {
                        units.add(new Unit(tile, kind, elfPower));
                    }
                }
            }
        }

        StatusResult status() {
            boolean elves = false;
            boolean goblins = false;
            int hp = 0;
            for (Unit u : units) {
                if (u.hitpoints <= 0) continue;
                if (u.kind == KIND_ELF) elves = true;
                else goblins = true;
                hp += u.hitpoints;
            }
            return new StatusResult(hp, elves && goblins);
        }

        void removeTheDead() {
            units.removeIf(unit -> unit.hitpoints <= 0);
        }

        void removeUnit(Unit u) {
            u.tile.kind = KIND_SPACE;
            u.tile.unit = null;
            u.tile = null;
        }

        TickResult tick(boolean stopOnElfDeath) {
            removeTheDead();
            units.sort(Comparator.comparingInt((Unit u) -> u.tile.y).thenComparingInt(u -> u.tile.x));
            for (Unit unit : units) {
                if (unit.hitpoints <= 0) continue;
                if (!unit.targets(this)) return new TickResult(false, false);
                unit.move(this);
                if (unit.attack(this) && stopOnElfDeath) return new TickResult(false, true);
            }
            return new TickResult(true, false);
        }
    }

    static class Tile {
        int kind;
        int x;
        int y;
        Map<Integer, Map<Integer, Tile>> map;
        Unit unit;

        Tile(int kind, int x, int y, Map<Integer, Map<Integer, Tile>> map) {
            this.kind = kind;
            this.x = x;
            this.y = y;
            this.map = map;
        }

        List<Tile> walkableNeighbors() {
            List<Tile> neighbors = new ArrayList<>();
            for (int[] offset : OFFSETS) {
                int newY = y + offset[1];
                int newX = x + offset[0];
                Map<Integer, Tile> row = map.get(newY);
                if (row != null) {
                    Tile n = row.get(newX);
                    if (n != null && n.kind == KIND_SPACE) {
                        neighbors.add(n);
                    }
                }
            }
            return neighbors;
        }
    }

    static class Unit {
        int kind;
        int hitpoints;
        int power;
        Tile tile;

        Unit(Tile tile, int kind, int elfPower) {
            this.kind = kind;
            this.hitpoints = DEFAULT_HITPOINTS;
            this.power = (kind != KIND_ELF) ? DEFAULT_POWER : elfPower;
            this.tile = tile;
            tile.unit = this;
        }

        boolean targets(Cave c) {
            for (Unit unit : c.units) {
                if (unit.kind != this.kind && unit.hitpoints > 0) {
                    return true;
                }
            }
            return false;
        }

        NextTileResult nextTile(Cave c) {
            List<Tile> targets = new ArrayList<>();
            int closestTargetDistance = Integer.MAX_VALUE;
            Map<Tile, Integer> distances = new HashMap<>();
            Map<Tile, Tile> path = new HashMap<>();
            findWalkableTiles(c.map, this.tile, distances, path);

            List<Unit> enemies = enemies(c);
            for (Unit enemy : enemies) {
                for (Tile target : enemy.tile.walkableNeighbors()) {
                    if (distances.containsKey(target) && distances.get(target) <= closestTargetDistance) {
                        if (distances.get(target) < closestTargetDistance) {
                            closestTargetDistance = distances.get(target);
                            targets.clear();
                        }
                        targets.add(target);
                    }
                }
            }
            targets.sort(Comparator.comparingInt((Tile t) -> t.y).thenComparingInt(t -> t.x));

            if (!targets.isEmpty()) {
                Tile target = targets.get(0);
                Tile current = target;
                while (path.get(current) != this.tile) {
                    current = path.get(current);
                }
                return new NextTileResult(current, target);
            }
            return new NextTileResult(null, null);
        }

        List<Unit> enemies(Cave c) {
            List<Unit> enemyList = new ArrayList<>();
            for (Unit unit : c.units) {
                if (unit.kind != this.kind && unit.hitpoints > 0) {
                    enemyList.add(unit);
                }
            }
            enemyList.sort(Comparator.comparingInt((Unit u) -> u.tile.y).thenComparingInt(u -> u.tile.x));
            return enemyList;
        }

        Unit enemyNeighbor(Cave c) {
            Unit target = null;
            for (int[] offset : OFFSETS) {
                int newY = tile.y + offset[1];
                int newX = tile.x + offset[0];
                Map<Integer, Tile> row = c.map.get(newY);
                if (row != null) {
                    Tile t = row.get(newX);
                    if (t != null && t.unit != null && t.unit.kind != this.kind && t.unit.hitpoints > 0) {
                        if (target == null || t.unit.hitpoints < target.hitpoints) {
                            target = t.unit;
                        }
                    }
                }
            }
            return target;
        }

        void move(Cave c) {
            if (enemyNeighbor(c) != null) return;
            NextTileResult nextTileResult = nextTile(c);
            Tile next = nextTileResult.next;
            if (next != null) {
                next.unit = this;
                next.kind = this.kind;
                this.tile.kind = KIND_SPACE;
                this.tile.unit = null;
                this.tile = next;
            }
        }

        boolean attack(Cave c) {
            Unit enemy = enemyNeighbor(c);
            if (enemy != null) {
                boolean killed = enemy.damage(c, this.power);
                return killed && enemy.kind == KIND_ELF;
            }
            return false;
        }

        boolean damage(Cave c, int damage) {
            this.hitpoints -= damage;
            if (this.hitpoints <= 0) {
                c.removeUnit(this);
                return true;
            }
            return false;
        }
    }

    static void findWalkableTiles(Map<Integer, Map<Integer, Tile>> map, Tile start, Map<Tile, Integer> distance, Map<Tile, Tile> cameFrom) {
        Queue<Tile> frontier = new LinkedList<>();
        frontier.add(start);
        distance.put(start, 0);
        cameFrom.put(start, null);

        while (!frontier.isEmpty()) {
            Tile current = frontier.poll();
            for (Tile next : current.walkableNeighbors()) {
                if (!distance.containsKey(next)) {
                    frontier.add(next);
                    distance.put(next, distance.get(current) + 1);
                    cameFrom.put(next, current);
                }
            }
        }
    }

    static boolean isUnit(int bit) {
        return (KIND_ELF | KIND_GOBLIN) == (KIND_ELF | KIND_GOBLIN | bit);
    }

    static int combat(List<String> input) {
        Cave cave = new Cave(input, DEFAULT_POWER);
        int i = 1;
        while (true) {
            StatusResult statusResult = cave.status();
            if (!statusResult.combat) return (i - 1) * statusResult.hp;
            TickResult tickResult = cave.tick(false);
            if (!tickResult.cleanRound) i--;
            i++;
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            System.out.println(combat(lines));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    record StatusResult(int hp, boolean combat) {}
    record TickResult(boolean cleanRound, boolean elfDied) {}
    record NextTileResult(Tile next, Tile target) {}
}
