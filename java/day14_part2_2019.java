
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class SpaceStoichiometry {

    private static final String FUEL = "FUEL";
    private static final String ORE = "ORE";

    private static class Reaction {
        Map<String, Long> inputs;
        String output;
        long outputQuantity;

        public Reaction(Map<String, Long> inputs, String output, long outputQuantity) {
            this.inputs = inputs;
            this.output = output;
            this.outputQuantity = outputQuantity;
        }
    }

    private static Map<String, Reaction> reactions;
    private static Map<String, Long> leftovers;

    public static void main(String[] args) {
        reactions = new HashMap<>();
        leftovers = new HashMap<>();

        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                parseReaction(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }

        long oreNeededForOneFuel = calculateOreNeeded(1);
        System.out.println("Minimum ORE required for 1 FUEL: " + oreNeededForOneFuel);

        long maxFuel = calculateMaxFuel(1000000000000L);
        System.out.println("Maximum FUEL produced with 1 trillion ORE: " + maxFuel);
    }

    private static void parseReaction(String line) {
        String[] parts = line.split(" => ");
        String[] inputParts = parts[0].split(", ");
        String[] outputParts = parts[1].split(" ");

        Map<String, Long> inputs = new HashMap<>();
        for (String inputPart : inputParts) {
            String[] chemicalParts = inputPart.split(" ");
            inputs.put(chemicalParts[1], Long.parseLong(chemicalParts[0]));
        }

        String output = outputParts[1];
        long outputQuantity = Long.parseLong(outputParts[0]);
        reactions.put(output, new Reaction(inputs, output, outputQuantity));
    }

    private static long calculateOreNeeded(long fuelAmount) {
        leftovers.clear();
        Map<String, Long> neededChemicals = new HashMap<>();
        neededChemicals.put(FUEL, fuelAmount);
        return calculateOre(neededChemicals);
    }

    private static long calculateOre(Map<String, Long> neededChemicals) {
        long oreNeeded = 0;
        while (!neededChemicals.isEmpty()) {
            String chemical = neededChemicals.keySet().iterator().next();
            long neededAmount = neededChemicals.remove(chemical);

            if (chemical.equals(ORE)) {
                oreNeeded += neededAmount;
                continue;
            }

            long leftoverAmount = leftovers.getOrDefault(chemical, 0L);
            if (leftoverAmount > 0) {
                long usedFromLeftover = Math.min(neededAmount, leftoverAmount);
                leftovers.put(chemical, leftoverAmount - usedFromLeftover);
                neededAmount -= usedFromLeftover;
            }

            if (neededAmount == 0) continue;

            Reaction reaction = reactions.get(chemical);
            long reactionCount = (long) Math.ceil((double) neededAmount / reaction.outputQuantity);

            for (Map.Entry<String, Long> inputEntry : reaction.inputs.entrySet()) {
                String inputChemical = inputEntry.getKey();
                long inputAmount = inputEntry.getValue() * reactionCount;
                neededChemicals.put(inputChemical, neededChemicals.getOrDefault(inputChemical, 0L) + inputAmount);
            }

            long producedAmount = reactionCount * reaction.outputQuantity;
            if (producedAmount > neededAmount) {
                leftovers.put(chemical, leftovers.getOrDefault(chemical, 0L) + producedAmount - neededAmount);
            }
        }
        return oreNeeded;
    }

    private static long calculateMaxFuel(long availableOre) {
        long low = 1;
        long high = availableOre;
        long maxFuel = 0;

        while (low <= high) {
            long mid = low + (high - low) / 2;
            long oreNeeded = calculateOreNeeded(mid);

            if (oreNeeded <= availableOre) {
                maxFuel = mid;
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return maxFuel;
    }
}
