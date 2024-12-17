
import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Main {

    static final int BROADCASTER = 0;
    static final int FLIP_FLOP = 1;
    static final int CONJUNCTION = 2;

    static class FlipFlop {
        String name;
        int moduleType;
        List<String> connectsTo;
        boolean state;

        public FlipFlop(String name) {
            this.name = name;
            this.moduleType = FLIP_FLOP;
            this.connectsTo = new ArrayList<>();
            this.state = false;
        }
    }

    static class Conjunction {
        String name;
        int moduleType;
        List<String> connectsTo;
        Map<String, Boolean> watches;

        public Conjunction(String name) {
            this.name = name;
            this.moduleType = CONJUNCTION;
            this.connectsTo = new ArrayList<>();
            this.watches = new HashMap<>();
        }
    }

    static class Broadcaster {
        String name;
        int moduleType;
        List<String> connectsTo;

        public Broadcaster(String name) {
            this.name = name;
            this.moduleType = BROADCASTER;
            this.connectsTo = new ArrayList<>();
        }
    }

    static class State {
        String from;
        String name;
        boolean pulse;

        public State(String from, String name, boolean pulse) {
            this.from = from;
            this.name = name;
            this.pulse = pulse;
        }
    }

    public static void handleLine(String line, Map<String, Object> connections) {
        if (line.contains("broadcaster")) {
            String[] parts = line.split(" -> ");
            Broadcaster module = new Broadcaster(parts[0]);
            module.connectsTo.addAll(Arrays.asList(parts[1].split(", ")));
            connections.put(module.name, module);
        } else if (line.contains("%")) {
            String[] parts = line.split(" -> ");
            FlipFlop module = new FlipFlop(parts[0].substring(1));
            module.connectsTo.addAll(Arrays.asList(parts[1].split(", ")));
            connections.put(module.name, module);
        } else {
            String[] parts = line.split(" -> ");
            Conjunction module = new Conjunction(parts[0].substring(1));
            module.connectsTo.addAll(Arrays.asList(parts[1].split(", ")));
            connections.put(module.name, module);
        }
    }

    public static void completeWatches(Map<String, Object> connections) {
        for (Object module : connections.values()) {
            if (module instanceof Conjunction) {
                Conjunction conj = (Conjunction) module;
                for (Object module2 : connections.values()) {
                    if (module2 instanceof FlipFlop) {
                        FlipFlop ff = (FlipFlop) module2;
                        for (String name : ff.connectsTo) {
                            if (name.equals(conj.name)) {
                                conj.watches.put(ff.name, false);
                            }
                        }
                    } else if (module2 instanceof Conjunction) {
                        Conjunction c = (Conjunction) module2;
                        for (String name : c.connectsTo) {
                            if (name.equals(conj.name)) {
                                conj.watches.put(c.name, false);
                            }
                        }
                    }
                }
                connections.put(conj.name, conj);
            }
        }
    }

    public static int[] simulatePress(Map<String, Object> connections, Map<String, Integer> loops, int pressNumber) {
        Queue<State> queue = new LinkedList<>();
        queue.add(new State("button", "broadcaster", false));
        int[] pulses = {1, 0};

        while (!queue.isEmpty()) {
            State currState = queue.poll();
            Object module = connections.get(currState.name);

            if (currState.name.equals("out")) continue;
            if (currState.name.equals("rx") && !currState.pulse) return null;

            boolean pulse = currState.pulse;

            if (module instanceof Broadcaster) {
                Broadcaster b = (Broadcaster) module;
                for (String name : b.connectsTo) {
                    queue.add(new State(b.name, name, pulse));
                    pulses[pulse ? 1 : 0]++;
                }
            } else if (module instanceof FlipFlop) {
                FlipFlop ff = (FlipFlop) module;
                if (!pulse) {
                    ff.state = !ff.state;
                    for (String name : ff.connectsTo) {
                        queue.add(new State(ff.name, name, ff.state));
                        pulses[ff.state ? 1 : 0]++;
                    }
                }
                connections.put(ff.name, ff);
            } else if (module instanceof Conjunction) {
                Conjunction conj = (Conjunction) module;
                conj.watches.put(currState.from, pulse);
                connections.put(conj.name, conj);

                boolean allTrue = true;
                for (boolean state : conj.watches.values()) {
                    if (!state) {
                        allTrue = false;
                        break;
                    }
                }

                for (String name : conj.connectsTo) {
                    queue.add(new State(conj.name, name, !allTrue));
                    pulses[!allTrue ? 1 : 0]++;
                }

                Integer currLoop = loops.get(conj.name);
                if (currLoop != null && !allTrue && currLoop == -1) {
                    loops.put(conj.name, pressNumber);
                }
            }
        }
        return pulses;
    }

    public static boolean connectsTo(String from, String to, Map<String, Object> connections) {
        Object module = connections.get(from);
        if (module instanceof Broadcaster) {
            return ((Broadcaster) module).connectsTo.contains(to);
        } else if (module instanceof FlipFlop) {
            return ((FlipFlop) module).connectsTo.contains(to);
        } else if (module instanceof Conjunction) {
            return ((Conjunction) module).connectsTo.contains(to);
        }
        return false;
    }

    public static void main(String[] args) {
        File file = new File("input.txt");
        Map<String, Object> connections = new HashMap<>();

        try (Scanner scanner = new Scanner(file)) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                handleLine(line, connections);
            }
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
            return;
        }

        completeWatches(connections);

        List<String> pxPrev = new ArrayList<>();
        for (String k : connections.keySet()) {
            if (connectsTo(k, "rx", connections)) {
                pxPrev.add(k);
            }
        }

        if (pxPrev.size() != 1) {
            throw new RuntimeException("Error: more than one pxPrev");
        }

        Conjunction conj;
        if (connections.get(pxPrev.get(0)) instanceof Conjunction) {
            conj = (Conjunction) connections.get(pxPrev.get(0));
        } else {
            throw new RuntimeException("Error: pxPrev is not a conjunction");
        }

        Map<String, Integer> loopLengths = new HashMap<>();
        for (String name : conj.watches.keySet()) {
            loopLengths.put(name, -1);
        }

        int pressNumber = 0;
        while (true) {
            pressNumber++;
            if(simulatePress(connections, loopLengths, pressNumber) == null) break;

            boolean complete = true;
            for (int length : loopLengths.values()) {
                if (length == -1) {
                    complete = false;
                    break;
                }
            }
            if (complete) break;
        }

        long sum = 1;
        for (int length : loopLengths.values()) {
            sum *= length;
        }

        System.out.println(sum);
    }
}
