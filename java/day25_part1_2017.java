
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class TuringMachine {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            char startState = ' ';
            int steps = 0;
            Map<Character, State> states = new HashMap<>();

            // Parse input
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("Begin in state")) {
                    startState = line.charAt(line.length() - 2);
                } else if (line.startsWith("Perform a diagnostic checksum after")) {
                    steps = Integer.parseInt(line.replaceAll("[^0-9]", ""));
                } else if (line.startsWith("In state")) {
                    char stateName = line.charAt(line.length() - 2);
                    State state = new State(stateName);

                    // Parse rules for 0
                    reader.readLine(); // Skip "If the current value is 0:"
                    line = reader.readLine().trim(); // "- Write the value X."
                    int writeValue0 = Integer.parseInt(line.substring(line.length() - 2, line.length() -1));
                    line = reader.readLine().trim(); // "- Move one slot to the [left/right]."
                    int move0 = line.contains("right") ? 1 : -1;
                    line = reader.readLine().trim(); // "- Continue with state X."
                    char nextState0 = line.charAt(line.length() - 2);
                    state.addRule(0, writeValue0, move0, nextState0);
                    
                    // Parse rules for 1
                    reader.readLine(); // Skip "If the current value is 1:"
                    line = reader.readLine().trim();
                    int writeValue1 = Integer.parseInt(line.substring(line.length() - 2, line.length() -1));
                    line = reader.readLine().trim();
                    int move1 = line.contains("right") ? 1 : -1;
                    line = reader.readLine().trim();
                    char nextState1 = line.charAt(line.length() - 2);
                    state.addRule(1, writeValue1, move1, nextState1);

                    states.put(stateName, state);
                }
            }
            reader.close();

            // Simulate Turing Machine
            Map<Integer, Integer> tape = new HashMap<>();
            int cursor = 0;
            char currentState = startState;

            for (int i = 0; i < steps; i++) {
                int currentValue = tape.getOrDefault(cursor, 0);
                State state = states.get(currentState);
                Rule rule = state.getRule(currentValue);

                tape.put(cursor, rule.writeValue);
                cursor += rule.move;
                currentState = rule.nextState;
            }

            // Calculate checksum
            int checksum = 0;
            for (int value : tape.values()) {
                checksum += value;
            }

            System.out.println(checksum);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    static class State {
        char name;
        Map<Integer, Rule> rules = new HashMap<>();

        public State(char name) {
            this.name = name;
        }

        public void addRule(int currentValue, int writeValue, int move, char nextState) {
            rules.put(currentValue, new Rule(writeValue, move, nextState));
        }

        public Rule getRule(int currentValue) {
            return rules.get(currentValue);
        }
    }

    static class Rule {
        int writeValue;
        int move;
        char nextState;

        public Rule(int writeValue, int move, char nextState) {
            this.writeValue = writeValue;
            this.move = move;
            this.nextState = nextState;
        }
    }
}
