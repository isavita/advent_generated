
class Main {
    static class Point {
        int x
        int y
        Point() {}
        Point(int x, int y) { this.x = x; this.y = y }
        boolean equals(Object o) {
            if (!(o instanceof Point)) return false
            Point p = (Point) o
            return x == p.x && y == p.y
        }
        int hashCode() {
            int r = 17
            r = 31 * r + x
            r = 31 * r + y
            return r
        }
    }

    static class State {
        Point room
        Map<Point, Integer> doorsPassed
        State(Point room, Map<Point, Integer> doorsPassed) {
            this.room = room
            this.doorsPassed = doorsPassed
        }
    }

    static Map<Point, Integer> buildMap(String directions) {
        Deque<State> stack = new ArrayDeque<>()
        Map<Point, Integer> rooms = new HashMap<>()
        Point currentRoom = new Point(0, 0)
        Map<Point, Integer> doorsPassed = new HashMap<>()
        doorsPassed.put(currentRoom, 0)

        for (int i = 1; i < directions.length() - 1; i++) {
            char ch = directions.charAt(i)
            if (ch == '(') {
                stack.push(new State(new Point(currentRoom.x, currentRoom.y), new HashMap<>(doorsPassed)))
            } else if (ch == '|') {
                State top = stack.peek()
                currentRoom = top.room
                doorsPassed = new HashMap<>(top.doorsPassed)
            } else if (ch == ')') {
                State top = stack.pop()
                currentRoom = top.room
                doorsPassed = new HashMap<>(top.doorsPassed)
            } else {
                int dx = 0, dy = 0
                switch (ch) {
                    case 'N': dy = -1; break
                    case 'E': dx = 1; break
                    case 'S': dy = 1; break
                    case 'W': dx = -1; break
                }
                Point next = new Point(currentRoom.x + dx, currentRoom.y + dy)
                int currentDoors = doorsPassed.getOrDefault(currentRoom, 0)
                int nd = currentDoors + 1
                doorsPassed.put(next, nd)
                Integer existing = rooms.get(next)
                if (existing == null || nd < existing) {
                    rooms.put(next, nd)
                }
                currentRoom = next
            }
        }
        return rooms
    }

    static void main(String[] args) {
        File f = new File("input.txt")
        if (!f.exists()) {
            System.exit(1)
        }
        String directions = f.getText('UTF-8').trim()
        Map<Point, Integer> rooms = buildMap(directions)
        int maxDoors = 0
        long roomsWith1000 = 0
        for (Integer v : rooms.values()) {
            if (v > maxDoors) maxDoors = v
            if (v >= 1000) roomsWith1000++
        }
        println(maxDoors)
        println(roomsWith1000)
    }
}

Main.main(null)
