
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CrossedWires {

    public static void main(String[] args) {
        Map<String, Integer> wireValues = new HashMap<>();
        Map<String, String> gateInstructions = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            boolean initialValuesSection = true;

            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                if (initialValuesSection) {
                    if (line.contains(":")) {
                        String[] parts = line.split(":");
                        String wireName = parts[0].trim();
                        int value = Integer.parseInt(parts[1].trim());
                        wireValues.put(wireName, value);
                    } else {
                        initialValuesSection = false;
                        gateInstructions.put(line, null); // Placeholder - will be overwritten
                    }
                }

                if (!initialValuesSection) {
                    gateInstructions.put(line, null);
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            return;
        }
       
        Map<String, Integer> calculatedValues = new HashMap<>(wireValues);  // Copy initial values

        Pattern gatePattern = Pattern.compile("([a-z0-9]+)\\s(AND|OR|XOR)\\s([a-z0-9]+)\\s->\\s([a-z0-9]+)");
        Pattern singleInputGatePattern = Pattern.compile("([a-z0-9]+)\\s->\\s([a-z0-9]+)");


        while (true) {
            boolean changed = false;
            for (String instruction : gateInstructions.keySet()) {
                if (calculatedValues.containsKey(instruction.split("->")[1].trim())) {
                    continue; // Already calculated this wire
                }

                Matcher matcher = gatePattern.matcher(instruction);
                if (matcher.find()) {
                    String input1 = matcher.group(1);
                    String operation = matcher.group(2);
                    String input2 = matcher.group(3);
                    String output = matcher.group(4);

                    if (calculatedValues.containsKey(input1) && calculatedValues.containsKey(input2)) {
                        int val1 = calculatedValues.get(input1);
                        int val2 = calculatedValues.get(input2);
                        int result = 0;

                        switch (operation) {
                            case "AND":
                                result = val1 & val2;
                                break;
                            case "OR":
                                result = val1 | val2;
                                break;
                            case "XOR":
                                result = val1 ^ val2;
                                break;
                        }

                        calculatedValues.put(output, result);
                        changed = true;

                    }
                } else {
                    Matcher singleMatcher = singleInputGatePattern.matcher(instruction);
                    if(singleMatcher.find()) {
                        String input = singleMatcher.group(1);
                        String output = singleMatcher.group(2);

                        if (calculatedValues.containsKey(input)) {
                            int val = calculatedValues.get(input);
                            calculatedValues.put(output, val);
                            changed = true;
                        }
                    }
                }
            }
            if (!changed) {
                break; // No changes in this iteration, so stop
            }

        }
        
        StringBuilder binaryString = new StringBuilder();
        for (int i = 0; calculatedValues.containsKey("z" + String.format("%02d", i)); ++i) {
            binaryString.insert(0, calculatedValues.get("z" + String.format("%02d", i)));
        }

        long decimalValue = 0;
        try {
            decimalValue = Long.parseLong(binaryString.toString(), 2);
        }
        catch (NumberFormatException e)
        {
            //Handle cases where there are no z wires or something prevents parsing
            decimalValue = 0;
        }
        
        System.out.println(decimalValue);
    }
}
