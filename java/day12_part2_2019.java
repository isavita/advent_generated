
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NBodyProblem {

    static class Moon {
        int[] pos;
        int[] vel;

        public Moon(int[] pos) {
            this.pos = pos;
            this.vel = new int[3];
        }

        public int potentialEnergy() {
            return Math.abs(pos[0]) + Math.abs(pos[1]) + Math.abs(pos[2]);
        }

        public int kineticEnergy() {
            return Math.abs(vel[0]) + Math.abs(vel[1]) + Math.abs(vel[2]);
        }

        public int totalEnergy() {
            return potentialEnergy() * kineticEnergy();
        }

        @Override
        public String toString() {
            return "pos=<x=" + pos[0] + ", y=" + pos[1] + ", z=" + pos[2] +
                    ">, vel=<x=" + vel[0] + ", y=" + vel[1] + ", z=" + vel[2] + ">";
        }
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<Moon> moons = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.substring(1, line.length() - 1).split(", ");
                int[] pos = new int[3];
                for (int i = 0; i < 3; i++) {
                    pos[i] = Integer.parseInt(parts[i].split("=")[1]);
                }
                moons.add(new Moon(pos));
            }

            // Part 1
            List<Moon> moonsPart1 = new ArrayList<>();
            for (Moon moon : moons) {
                moonsPart1.add(new Moon(Arrays.copyOf(moon.pos, 3)));
            }
            simulate(moonsPart1, 1000);
            int totalEnergy = moonsPart1.stream().mapToInt(Moon::totalEnergy).sum();
            System.out.println("Total energy after 1000 steps: " + totalEnergy);

            // Part 2
            long steps = findCycle(moons);
            System.out.println("Steps to repeat a previous state: " + steps);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void simulate(List<Moon> moons, int steps) {
        for (int step = 0; step < steps; step++) {
            applyGravity(moons);
            applyVelocity(moons);
        }
    }

    static void applyGravity(List<Moon> moons) {
        for (int i = 0; i < moons.size(); i++) {
            for (int j = i + 1; j < moons.size(); j++) {
                Moon moon1 = moons.get(i);
                Moon moon2 = moons.get(j);
                for (int k = 0; k < 3; k++) {
                    if (moon1.pos[k] < moon2.pos[k]) {
                        moon1.vel[k]++;
                        moon2.vel[k]--;
                    } else if (moon1.pos[k] > moon2.pos[k]) {
                        moon1.vel[k]--;
                        moon2.vel[k]++;
                    }
                }
            }
        }
    }

    static void applyVelocity(List<Moon> moons) {
        for (Moon moon : moons) {
            for (int i = 0; i < 3; i++) {
                moon.pos[i] += moon.vel[i];
            }
        }
    }

    static long findCycle(List<Moon> moons) {
        long[] cycleLengths = new long[3];
        for (int axis = 0; axis < 3; axis++) {
            Map<String, Long> states = new HashMap<>();
            List<Moon> currentMoons = new ArrayList<>();
            for (Moon moon : moons) {
                currentMoons.add(new Moon(Arrays.copyOf(moon.pos, 3)));
            }

            long step = 0;
            while (true) {
                String state = getState(currentMoons, axis);
                if (states.containsKey(state)) {
                    cycleLengths[axis] = step - states.get(state);
                    break;
                }
                states.put(state, step);
                applyGravity(currentMoons);
                applyVelocity(currentMoons);
                step++;
            }
        }
        return lcm(cycleLengths);
    }

    static String getState(List<Moon> moons, int axis) {
        StringBuilder sb = new StringBuilder();
        for (Moon moon : moons) {
            sb.append(moon.pos[axis]).append(",").append(moon.vel[axis]).append(",");
        }
        return sb.toString();
    }

    static long gcd(long a, long b) {
        while (b > 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    static long lcm(long a, long b) {
        return a * (b / gcd(a, b));
    }

    static long lcm(long[] input) {
        long result = input[0];
        for (int i = 1; i < input.length; i++) {
            result = lcm(result, input[i]);
        }
        return result;
    }
}
