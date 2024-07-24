
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {
    static class Module {
        String name;
        char prefix;
        List<String> destinations;
        boolean state;
        Map<String, PulseValue> memory = new HashMap<>();
    }

    static class Pulse {
        PulseValue value;
        String fromName;
        String toName;
    }

    enum PulseValue {
        Low, High
    }

    static final char FlipFlop = '%';
    static final char Conjunction = '&';

    static Map<String, Module> parseInput(List<String> input) {
        Map<String, Module> modules = new HashMap<>();
        for (String line : input) {
            String[] parts = line.split(" -> ");
            Module module = new Module();
            module.destinations = Arrays.asList(parts[1].split(", "));
            module.prefix = parts[0].charAt(0);
            module.name = module.prefix == FlipFlop || module.prefix == Conjunction ? parts[0].substring(1) : parts[0];
            modules.put(module.name, module);
        }

        for (Module module : modules.values()) {
            for (String destName : module.destinations) {
                if (modules.containsKey(destName) && modules.get(destName).prefix == Conjunction) {
                    modules.get(destName).memory.put(module.name, PulseValue.Low);
                }
            }
        }
        return modules;
    }

    static int[] pushButton(Map<String, Module> modules, Pulse startPulse, int numCycle) {
        int[] counts = new int[2]; // counts[0] for Low, counts[1] for High
        Queue<Pulse> pulseQueue = new LinkedList<>();

        for (int i = 0; i < numCycle; i++) {
            pulseQueue.offer(startPulse);
            while (!pulseQueue.isEmpty()) {
                Pulse pulse = pulseQueue.poll();
                counts[pulse.value == PulseValue.Low ? 0 : 1]++;

                if (!modules.containsKey(pulse.toName)) continue;

                Module module = modules.get(pulse.toName);
                PulseValue newPulseValue;

                switch (module.prefix) {
                    case FlipFlop:
                        if (pulse.value == PulseValue.Low) {
                            module.state = !module.state;
                            newPulseValue = module.state ? PulseValue.High : PulseValue.Low;
                        } else {
                            continue;
                        }
                        break;
                    case Conjunction:
                        module.memory.put(pulse.fromName, pulse.value);
                        newPulseValue = module.memory.values().stream().allMatch(v -> v == PulseValue.High) ? PulseValue.Low : PulseValue.High;
                        break;
                    default:
                        newPulseValue = pulse.value;
                }

                for (String destName : module.destinations) {
                    pulseQueue.offer(new Pulse() {{
                        value = newPulseValue;
                        fromName = pulse.toName;
                        toName = destName;
                    }});
                }
            }
        }
        return counts;
    }

    static int solve(List<String> input) {
        Pulse startPulse = new Pulse() {{
            value = PulseValue.Low;
            fromName = "button";
            toName = "broadcaster";
        }};
        int numCycle = 1000;

        Map<String, Module> modules = parseInput(input);
        int[] counts = pushButton(modules, startPulse, numCycle);

        return counts[0] * counts[1];
    }

    public static void main(String[] args) throws IOException {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        System.out.println(solve(input));
    }
}
