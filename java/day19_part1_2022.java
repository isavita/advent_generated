
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(new File("input.txt").toPath()).trim();
        System.out.println(part1(input));
    }

    static int part1(String input) {
        List<Blueprint> blueprints = parseInput(input);
        int sum = 0;
        for (Blueprint bp : blueprints) {
            State st = new State(bp);
            int geodesMade = st.calcMostGeodes(0, new HashMap<>(), 24, 24);
            sum += bp.id * geodesMade;
        }
        return sum;
    }

    static class Blueprint {
        int id;
        int oreForOreRobot;
        int oreForClayRobot;
        int oreForObsidianRobot;
        int clayForObsidianRobot;
        int oreForGeodeRobot;
        int obsidianForGeodeRobot;

        public Blueprint(int id, int oreForOreRobot, int oreForClayRobot, int oreForObsidianRobot, int clayForObsidianRobot, int oreForGeodeRobot, int obsidianForGeodeRobot) {
            this.id = id;
            this.oreForOreRobot = oreForOreRobot;
            this.oreForClayRobot = oreForClayRobot;
            this.oreForObsidianRobot = oreForObsidianRobot;
            this.clayForObsidianRobot = clayForObsidianRobot;
            this.oreForGeodeRobot = oreForGeodeRobot;
            this.obsidianForGeodeRobot = obsidianForGeodeRobot;
        }
    }

    static class State {
        Blueprint blueprint;
        int ore;
        int clay;
        int obsidian;
        int geode;
        int oreRobots;
        int clayRobots;
        int obsidianRobots;
        int geodeRobots;

        public State(Blueprint blueprint) {
            this.blueprint = blueprint;
            this.oreRobots = 1;
        }

        void farm() {
            ore += oreRobots;
            clay += clayRobots;
            obsidian += obsidianRobots;
            geode += geodeRobots;
        }

        String hash(int time) {
            return String.format("%d,%d,%d,%d,%d,%d,%d,%d,%d", time, ore, clay, obsidian, geode, oreRobots, clayRobots, obsidianRobots, geodeRobots);
        }

        State copy() {
            State copy = new State(blueprint);
            copy.ore = ore;
            copy.clay = clay;
            copy.obsidian = obsidian;
            copy.geode = geode;
            copy.oreRobots = oreRobots;
            copy.clayRobots = clayRobots;
            copy.obsidianRobots = obsidianRobots;
            copy.geodeRobots = geodeRobots;
            return copy;
        }

        int calcMostGeodes(int time, Map<String, Integer> memo, int totalTime, int earliestGeode) {
            if (time == totalTime) {
                return geode;
            }

            String h = hash(time);
            if (memo.containsKey(h)) {
                return memo.get(h);
            }

            if (geode == 0 && time > earliestGeode) {
                return 0;
            }

            int mostGeodes = geode;

            if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
                State cp = copy();
                cp.farm();
                cp.ore -= blueprint.oreForGeodeRobot;
                cp.obsidian -= blueprint.obsidianForGeodeRobot;
                cp.geodeRobots++;
                if (cp.geodeRobots == 1) {
                    earliestGeode = Math.min(earliestGeode, time + 1);
                }
                mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
                memo.put(h, mostGeodes);
                return mostGeodes;
            }

            if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
                State cp = copy();
                cp.ore -= blueprint.oreForOreRobot;
                cp.farm();
                cp.oreRobots++;
                mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }
            if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
                State cp = copy();
                cp.ore -= blueprint.oreForClayRobot;
                cp.farm();
                cp.clayRobots++;
                mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }
            if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
                State cp = copy();
                cp.ore -= blueprint.oreForObsidianRobot;
                cp.clay -= blueprint.clayForObsidianRobot;
                cp.farm();
                cp.obsidianRobots++;
                mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }

            State cp = copy();
            cp.farm();
            mostGeodes = Math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));

            memo.put(h, mostGeodes);
            return mostGeodes;
        }
    }

    static List<Blueprint> parseInput(String input) {
        Pattern pattern = Pattern.compile("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.");
        List<Blueprint> blueprints = new java.util.ArrayList<>();
        for (String line : input.split("\n")) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                int id = Integer.parseInt(matcher.group(1));
                int oreForOreRobot = Integer.parseInt(matcher.group(2));
                int oreForClayRobot = Integer.parseInt(matcher.group(3));
                int oreForObsidianRobot = Integer.parseInt(matcher.group(4));
                int clayForObsidianRobot = Integer.parseInt(matcher.group(5));
                int oreForGeodeRobot = Integer.parseInt(matcher.group(6));
                int obsidianForGeodeRobot = Integer.parseInt(matcher.group(7));
                blueprints.add(new Blueprint(id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot));
            }
        }
        return blueprints;
    }
}
