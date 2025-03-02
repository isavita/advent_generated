
import java.util.PriorityQueue

enum UnitType {
    ELF, GOBLIN
}

class Unit {
    UnitType type
    int hp = 200
    int attackPower = 3
    int row
    int col

    Unit(UnitType type, int row, int col) {
        this.type = type
        this.row = row
        this.col = col
    }

    @Override
    String toString() {
        return "${type.name()}(${hp})@(${row},${col})"
    }
}

class Node implements Comparable<Node> {
    int row
    int col
    int distance
    int priority

    Node(int row, int col, int distance, int priority) {
        this.row = row
        this.col = col
        this.distance = distance
        this.priority = priority
    }

    @Override
    int compareTo(Node other) {
        if (distance != other.distance) {
            return distance - other.distance
        }
        return priority - other.priority
    }
}

class Combat {
    char[][] map
    List<Unit> units = []
    int rows
    int cols
    int rounds = 0

    Combat(List<String> input) {
        rows = input.size()
        cols = input[0].length()
        map = new char[rows][cols]

        for (int i = 0; i < rows; i++) {
            String row = input[i]
            for (int j = 0; j < cols; j++) {
                char c = row[j]
                map[i][j] = c
                if (c == 'G') {
                    units << new Unit(UnitType.GOBLIN, i, j)
                } else if (c == 'E') {
                    units << new Unit(UnitType.ELF, i, j)
                }
            }
        }
    }


    boolean round() {
        units.sort { a, b -> a.row == b.row ? a.col <=> b.col : a.row <=> b.row }

        for (int i = 0; i < units.size(); i++) {
            Unit unit = units[i]
            if (unit.hp <= 0) {
                continue
            }

            List<Unit> targets = findTargets(unit)
            if (targets.isEmpty()) {
                return false
            }

            move(unit, targets)
            attack(unit, targets)

            
        }
        rounds++
        return true
    }

    List<Unit> findTargets(Unit unit) {
        if (unit.type == UnitType.ELF) {
            return units.findAll { it.type == UnitType.GOBLIN && it.hp > 0 }
        } else {
            return units.findAll { it.type == UnitType.ELF && it.hp > 0 }
        }
    }

    void move(Unit unit, List<Unit> targets) {
        if (isInRange(unit, targets)) {
            return
        }

        List<Point> inRange = []
        targets.each { target ->
            getAdjacentPoints(target.row, target.col).each { point ->
                if (map[point.row][point.col] == '.' && !isUnitAt(point.row, point.col)) {
                    inRange << point
                }
            }
        }

        if (inRange.isEmpty()) {
            return
        }

        Point bestTarget = findNearest(unit, inRange)

        if (bestTarget != null) {
            Point nextStep = findNextStep(unit, bestTarget)
            if (nextStep != null) {
                map[unit.row][unit.col] = '.'
                unit.row = nextStep.row
                unit.col = nextStep.col
                map[unit.row][unit.col] = unit.type == UnitType.ELF ? 'E' : 'G'
            }
        }
    }

    Point findNearest(Unit unit, List<Point> targets) {
        int[][] dist = new int[rows][cols]
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                dist[i][j] = Integer.MAX_VALUE
            }
        }

        PriorityQueue<Node> queue = new PriorityQueue<>()
        queue.add(new Node(unit.row, unit.col, 0, unit.row * cols + unit.col))
        dist[unit.row][unit.col] = 0

        while (!queue.isEmpty()) {
            Node current = queue.poll()
            int row = current.row
            int col = current.col
            int distance = current.distance

            if (distance > dist[row][col]) {
                continue
            }

            getAdjacentPoints(row, col).each { neighbor ->
                if (map[neighbor.row][neighbor.col] == '.' && dist[neighbor.row][neighbor.col] == Integer.MAX_VALUE && !isUnitAt(neighbor.row, neighbor.col)) {
                    dist[neighbor.row][neighbor.col] = distance + 1
                    queue.add(new Node(neighbor.row, neighbor.col, distance + 1, neighbor.row * cols + neighbor.col))
                }
            }
        }

        Point nearest = null
        int minDist = Integer.MAX_VALUE

        targets.each { target ->
            if (dist[target.row][target.col] < minDist) {
                minDist = dist[target.row][target.col]
                nearest = target
            } else if (dist[target.row][target.col] == minDist && nearest != null) {
                if (target.row < nearest.row || (target.row == nearest.row && target.col < nearest.col)) {
                    nearest = target
                }
            }
        }
        return nearest
    }

    Point findNextStep(Unit unit, Point target) {
        int[][] dist = new int[rows][cols]
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                dist[i][j] = Integer.MAX_VALUE
            }
        }

        PriorityQueue<Node> queue = new PriorityQueue<>()
        queue.add(new Node(target.row, target.col, 0, target.row * cols + target.col))
        dist[target.row][target.col] = 0

        while (!queue.isEmpty()) {
            Node current = queue.poll()
            int row = current.row
            int col = current.col
            int distance = current.distance

            if (distance > dist[row][col]) {
                continue
            }

            getAdjacentPoints(row, col).each { neighbor ->
                if (map[neighbor.row][neighbor.col] == '.' && !isUnitAt(neighbor.row, neighbor.col) && dist[neighbor.row][neighbor.col] == Integer.MAX_VALUE) {
                    dist[neighbor.row][neighbor.col] = distance + 1
                    queue.add(new Node(neighbor.row, neighbor.col, distance + 1, neighbor.row * cols + neighbor.col))
                }
            }
        }

        List<Point> possibleSteps = getAdjacentPoints(unit.row, unit.col).findAll {
            map[it.row][it.col] == '.' && !isUnitAt(it.row,it.col) && dist[it.row][it.col] != Integer.MAX_VALUE
        }

        if (possibleSteps.isEmpty()) {
            return null
        }

        possibleSteps.sort { a, b -> a.row == b.row ? a.col <=> b.col : a.row <=> b.row }
        int minDist = Integer.MAX_VALUE;
        Point bestStep = null;

        for (Point step : possibleSteps) {
           if(dist[step.row][step.col] < minDist){
               minDist = dist[step.row][step.col];
               bestStep = step;
           }
        }

        return bestStep
    }

    void attack(Unit unit, List<Unit> targets) {
        List<Unit> inRange = findUnitsInRange(unit)
        if (inRange.isEmpty()) {
            return
        }

        Unit target = inRange.sort { a, b -> a.hp == b.hp ? a.row == b.row ? a.col <=> b.col : a.row <=> b.row : a.hp <=> b.hp }[0]
        target.hp -= unit.attackPower

        if (target.hp <= 0) {
            map[target.row][target.col] = '.'
        }
    }


    List<Unit> findUnitsInRange(Unit unit) {
        List<Unit> inRange = []
        getAdjacentPoints(unit.row, unit.col).each { point ->
            Unit other = getUnitAt(point.row, point.col)
            if (other != null && other.type != unit.type && other.hp > 0) {
                inRange << other
            }
        }
        return inRange
    }

    boolean isInRange(Unit unit, List<Unit> targets) {
        return targets.any { target ->
            Math.abs(unit.row - target.row) + Math.abs(unit.col - target.col) == 1
        }
    }


    Unit getUnitAt(int row, int col) {
        return units.find { it.row == row && it.col == col && it.hp > 0 }
    }

    boolean isUnitAt(int row, int col) {
        return units.any { it.row == row && it.col == col && it.hp > 0 }
    }

    List<Point> getAdjacentPoints(int row, int col) {
        List<Point> adj = []
        if (row > 0) adj << new Point(row - 1, col)
        if (row < rows - 1) adj << new Point(row + 1, col)
        if (col > 0) adj << new Point(row, col - 1)
        if (col < cols - 1) adj << new Point(row, col + 1)
        return adj
    }

    int calculateOutcome() {
        int totalHp = units.findAll { it.hp > 0 }.sum { it.hp }
        return rounds * totalHp
    }

    void printMap() {
        for (int i = 0; i < rows; i++) {
            println new String(map[i]) + "   " + units.findAll { it.row == i && it.hp > 0 }.join(", ")
        }
        println()
    }
}

class Point {
    int row
    int col

    Point(int row, int col) {
        this.row = row
        this.col = col
    }
}

static void main(String[] args) {
    File file = new File("input.txt")
    List<String> input = file.readLines()

    Combat combat = new Combat(input)
    //combat.printMap()

    while (combat.round()) {
       // combat.printMap()
    }
    //combat.printMap()

    println "Outcome: " + combat.calculateOutcome()
}
