
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

class Solution {

    static class Blueprint {
        int id;
        int oreCost;
        int clayOreCost;
        int obsidianOreCost;
        int obsidianClayCost;
        int geodeOreCost;
        int geodeObsidianCost;
    }

    static class State {
        int ore, clay, obsidian, geode;
        int oreRobots, clayRobots, obsidianRobots, geodeRobots;
        int timeLeft;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return ore == state.ore && clay == state.clay && obsidian == state.obsidian && geode == state.geode && oreRobots == state.oreRobots && clayRobots == state.clayRobots && obsidianRobots == state.obsidianRobots && geodeRobots == state.geodeRobots && timeLeft == state.timeLeft;
        }

        @Override
        public int hashCode() {
            return Objects.hash(ore, clay, obsidian, geode, oreRobots, clayRobots, obsidianRobots, geodeRobots, timeLeft);
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("input.txt"));
        List<Blueprint> blueprints = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            Blueprint b = new Blueprint();
            String[] parts = line.split(" ");
            b.id = Integer.parseInt(parts[1].substring(0, parts[1].length() - 1));
            b.oreCost = Integer.parseInt(parts[6]);
            b.clayOreCost = Integer.parseInt(parts[12]);
            b.obsidianOreCost = Integer.parseInt(parts[18]);
            b.obsidianClayCost = Integer.parseInt(parts[21]);
            b.geodeOreCost = Integer.parseInt(parts[27]);
            b.geodeObsidianCost = Integer.parseInt(parts[30]);
            blueprints.add(b);
        }

        State init = new State();
        init.oreRobots = 1;
        init.timeLeft = 32;

        int prod = 1;
        for (int i = 0; i < 3; i++) {
            prod *= maxGeode(blueprints.get(i), init);
        }
        System.out.println(prod);
    }

    static int maxGeode(Blueprint b, State st) {
        int max = 0;
        Queue<State> q = new LinkedList<>();
        q.add(st);
        Set<State> visited = new HashSet<>();

        while (!q.isEmpty()) {
            State s = q.poll();
            max = Math.max(max, s.geode);
            if (s.timeLeft == 0) continue;

            int o = Math.max(Math.max(b.oreCost, b.clayOreCost), Math.max(b.obsidianOreCost, b.geodeOreCost));
            if (s.oreRobots >= o) s.oreRobots = o;
            if (s.clayRobots >= b.obsidianClayCost) s.clayRobots = b.obsidianClayCost;
            if (s.obsidianRobots >= b.geodeObsidianCost) s.obsidianRobots = b.geodeObsidianCost;

            int maxOre = s.timeLeft * o - s.oreRobots * (s.timeLeft - 1);
            if (s.ore >= maxOre) s.ore = maxOre;
            int maxClay = s.timeLeft * b.obsidianClayCost - s.clayRobots * (s.timeLeft - 1);
            if (s.clay >= maxClay) s.clay = maxClay;
            int maxObsidian = s.timeLeft * b.geodeObsidianCost - s.obsidianRobots * (s.timeLeft - 1);
            if (s.obsidian >= maxObsidian) s.obsidian = maxObsidian;

            if (visited.contains(s)) continue;
            visited.add(s);

            State next = new State();
            next.ore = s.ore + s.oreRobots;
            next.clay = s.clay + s.clayRobots;
            next.obsidian = s.obsidian + s.obsidianRobots;
            next.geode = s.geode + s.geodeRobots;
            next.oreRobots = s.oreRobots;
            next.clayRobots = s.clayRobots;
            next.obsidianRobots = s.obsidianRobots;
            next.geodeRobots = s.geodeRobots;
            next.timeLeft = s.timeLeft - 1;
            q.add(next);

            if (s.ore >= b.oreCost) {
                next = new State();
                next.ore = s.ore - b.oreCost + s.oreRobots;
                next.clay = s.clay + s.clayRobots;
                next.obsidian = s.obsidian + s.obsidianRobots;
                next.geode = s.geode + s.geodeRobots;
                next.oreRobots = s.oreRobots + 1;
                next.clayRobots = s.clayRobots;
                next.obsidianRobots = s.obsidianRobots;
                next.geodeRobots = s.geodeRobots;
                next.timeLeft = s.timeLeft - 1;
                q.add(next);
            }

            if (s.ore >= b.clayOreCost) {
                next = new State();
                next.ore = s.ore - b.clayOreCost + s.oreRobots;
                next.clay = s.clay + s.clayRobots;
                next.obsidian = s.obsidian + s.obsidianRobots;
                next.geode = s.geode + s.geodeRobots;
                next.oreRobots = s.oreRobots;
                next.clayRobots = s.clayRobots + 1;
                next.obsidianRobots = s.obsidianRobots;
                next.geodeRobots = s.geodeRobots;
                next.timeLeft = s.timeLeft - 1;
                q.add(next);
            }

            if (s.ore >= b.obsidianOreCost && s.clay >= b.obsidianClayCost) {
                next = new State();
                next.ore = s.ore - b.obsidianOreCost + s.oreRobots;
                next.clay = s.clay - b.obsidianClayCost + s.clayRobots;
                next.obsidian = s.obsidian + s.obsidianRobots;
                next.geode = s.geode + s.geodeRobots;
                next.oreRobots = s.oreRobots;
                next.clayRobots = s.clayRobots;
                next.obsidianRobots = s.obsidianRobots + 1;
                next.geodeRobots = s.geodeRobots;
                next.timeLeft = s.timeLeft - 1;
                q.add(next);
            }

            if (s.ore >= b.geodeOreCost && s.obsidian >= b.geodeObsidianCost) {
                next = new State();
                next.ore = s.ore - b.geodeOreCost + s.oreRobots;
                next.clay = s.clay + s.clayRobots;
                next.obsidian = s.obsidian - b.geodeObsidianCost + s.obsidianRobots;
                next.geode = s.geode + s.geodeRobots;
                next.oreRobots = s.oreRobots;
                next.clayRobots = s.clayRobots;
                next.obsidianRobots = s.obsidianRobots;
                next.geodeRobots = s.geodeRobots + 1;
                next.timeLeft = s.timeLeft - 1;
                q.add(next);
            }
        }
        return max;
    }
}
