
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.function.LongUnaryOperator;
import java.util.stream.Collectors;

public class MonkeyInTheMiddle {

    static class Monkey {
        List<Long> items;
        LongUnaryOperator operation;
        int testDivisor;
        int trueTarget;
        int falseTarget;
        long inspectionCount = 0;

        public Monkey(List<Long> items, LongUnaryOperator operation, int testDivisor, int trueTarget, int falseTarget) {
            this.items = items;
            this.operation = operation;
            this.testDivisor = testDivisor;
            this.trueTarget = trueTarget;
            this.falseTarget = falseTarget;
        }
    }

    public static void main(String[] args) {
        try {
            List<Monkey> monkeys = readMonkeys("input.txt");
            long monkeyBusiness = calculateMonkeyBusiness(monkeys, 20, true); // Part 1
            System.out.println("Part 1 - Monkey Business after 20 rounds: " + monkeyBusiness);

            monkeys = readMonkeys("input.txt"); // Reset monkeys for part 2
            monkeyBusiness = calculateMonkeyBusiness(monkeys, 10000, false); // Part 2
            System.out.println("Part 2 - Monkey Business after 10000 rounds: " + monkeyBusiness);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<Monkey> readMonkeys(String filename) throws IOException {
        List<Monkey> monkeys = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.trim().startsWith("Monkey")) {
                    // Starting items
                    line = reader.readLine();
                    List<Long> items = Arrays.stream(line.trim().substring("Starting items: ".length()).split(", "))
                            .map(Long::parseLong)
                            .collect(Collectors.toList());

                    // Operation
                    line = reader.readLine();
                    String operationLine = line.trim().substring("Operation: new = ".length());
                    LongUnaryOperator operation = createOperation(operationLine);

                    // Test
                    line = reader.readLine();
                    int testDivisor = Integer.parseInt(line.trim().substring("Test: divisible by ".length()));

                    // True target
                    line = reader.readLine();
                    int trueTarget = Integer.parseInt(line.trim().substring("If true: throw to monkey ".length()));

                    // False target
                    line = reader.readLine();
                    int falseTarget = Integer.parseInt(line.trim().substring("If false: throw to monkey ".length()));

                    monkeys.add(new Monkey(items, operation, testDivisor, trueTarget, falseTarget));
                }
            }
        }
        return monkeys;
    }


    private static LongUnaryOperator createOperation(String operationLine) {
        String[] parts = operationLine.split(" ");
        String operand = parts[0];
        String operator = parts[1];
        String value = parts[2];

        return old -> {
            long val;
            if ("old".equals(value)) {
                val = old;
            } else {
                val = Long.parseLong(value);
            }

            switch (operator) {
                case "+":
                    return old + val;
                case "*":
                    return old * val;
                default:
                    throw new IllegalArgumentException("Invalid operator: " + operator);
            }
        };
    }

    private static long calculateMonkeyBusiness(List<Monkey> monkeys, int rounds, boolean divideByThree) {
        long lcm = 1;
        for (Monkey monkey : monkeys) {
            lcm = lcm * monkey.testDivisor / gcd(lcm, monkey.testDivisor);  // Calculate LCM for part 2
        }

        for (int round = 0; round < rounds; round++) {
            for (Monkey monkey : monkeys) {
                for (long item : monkey.items) {
                    monkey.inspectionCount++;
                    long worryLevel = monkey.operation.applyAsLong(item);
                    if (divideByThree) {
                        worryLevel /= 3;
                    } else {
                        worryLevel %= lcm; // Keep worry levels manageable using LCM
                    }

                    int targetMonkey = (worryLevel % monkey.testDivisor == 0) ? monkey.trueTarget : monkey.falseTarget;
                    monkeys.get(targetMonkey).items.add(worryLevel);
                }
                monkey.items.clear();
            }
        }

        return monkeys.stream()
                .mapToLong(m -> m.inspectionCount)
                .boxed()
                .sorted(Comparator.reverseOrder())
                .limit(2)
                .reduce(1L, (a, b) -> a * b);
    }

    private static long gcd(long a, long b) {
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
}
