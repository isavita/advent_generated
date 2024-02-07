
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class solution {
    static class Step {
        char id;
        int duration;

        public Step(char id, int duration) {
            this.id = id;
            this.duration = duration;
        }
    }

    public static void main(String[] args) {
        Map<Character, List<Character>> deps = new HashMap<>();
        Map<Character, Step> allSteps = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                char a = line.charAt(5);
                char b = line.charAt(36);
                if (!deps.containsKey(b)) {
                    deps.put(b, new ArrayList<>());
                }
                deps.get(b).add(a);
                if (!allSteps.containsKey(a)) {
                    allSteps.put(a, new Step(a, a - 'A' + 61));
                }
                if (!allSteps.containsKey(b)) {
                    allSteps.put(b, new Step(b, b - 'A' + 61));
                }
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        int timeTaken = simulateWork(deps, allSteps, 5, 60);
        System.out.println(timeTaken);
    }

    public static int simulateWork(Map<Character, List<Character>> deps, Map<Character, Step> allSteps, int numWorkers, int baseDuration) {
        int[] workers = new int[numWorkers];
        char[] tasks = new char[numWorkers];
        int time = 0;

        while (!allSteps.isEmpty()) {
            List<Character> available = new ArrayList<>();
            for (char step : allSteps.keySet()) {
                if (deps.get(step) == null || deps.get(step).size() == 0 && !isBeingWorkedOn(step, tasks)) {
                    available.add(step);
                }
            }
            available.sort(Character::compare);

            for (int i = 0; i < numWorkers; i++) {
                if (workers[i] == 0 && !available.isEmpty()) {
                    tasks[i] = available.get(0);
                    workers[i] = allSteps.get(available.get(0)).duration;
                    available.remove(0);
                }
            }

            int minDuration = findMinDuration(workers);
            for (int i = 0; i < numWorkers; i++) {
                if (workers[i] != 0) {
                    workers[i] -= minDuration;
                    if (workers[i] == 0) {
                        finishStep(deps, allSteps, tasks[i]);
                        tasks[i] = 0;
                    }
                }
            }
            time += minDuration;
        }

        return time;
    }

    public static boolean isBeingWorkedOn(char step, char[] tasks) {
        for (char task : tasks) {
            if (task == step) {
                return true;
            }
        }
        return false;
    }

    public static int findMinDuration(int[] durations) {
        int min = Integer.MAX_VALUE;
        for (int duration : durations) {
            if (duration > 0 && duration < min) {
                min = duration;
            }
        }
        return min;
    }

    public static void finishStep(Map<Character, List<Character>> deps, Map<Character, Step> allSteps, char step) {
        allSteps.remove(step);
        for (char s : allSteps.keySet()) {
            if (deps.get(s) != null) {
                deps.get(s).remove((Character) step);
            }
        }
    }
}
