
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MonkeyBusiness {

    static class Monkey {
        List<Long> items;
        char operation;
        long operand;
        int testDivisor;
        int trueMonkey;
        int falseMonkey;
        long inspectCount;

        public Monkey(List<Long> items, char operation, long operand, int testDivisor, int trueMonkey, int falseMonkey) {
            this.items = items;
            this.operation = operation;
            this.operand = operand;
            this.testDivisor = testDivisor;
            this.trueMonkey = trueMonkey;
            this.falseMonkey = falseMonkey;
            this.inspectCount = 0;
        }
    }

    public static void main(String[] args) {
        List<Monkey> monkeys = readMonkeys("input.txt");
        if (monkeys == null) {
            return;
        }

        for (int round = 0; round < 20; round++) {
            for (Monkey monkey : monkeys) {
                for (int i = 0; i < monkey.items.size(); i++) {
                    long item = monkey.items.get(i);
                    monkey.inspectCount++;

                    long newValue = performOperation(item, monkey.operation, monkey.operand);
                    newValue /= 3;

                    int targetMonkey = (newValue % monkey.testDivisor == 0) ? monkey.trueMonkey : monkey.falseMonkey;
                    monkeys.get(targetMonkey).items.add(newValue);
                }
                monkey.items.clear();
            }
        }

        long[] inspectCounts = monkeys.stream().mapToLong(m -> m.inspectCount).toArray();
        Arrays.sort(inspectCounts);
        long monkeyBusiness = inspectCounts[inspectCounts.length - 1] * inspectCounts[inspectCounts.length - 2];

        System.out.println(monkeyBusiness);
    }

    private static long performOperation(long oldValue, char operation, long operand) {
        if (operation == '+') {
            return oldValue + operand;
        } else if (operation == '*') {
            if (operand == -1) {
                return oldValue * oldValue;
            }
            return oldValue * operand;
        }
        return oldValue;
    }

    private static List<Monkey> readMonkeys(String filename) {
        List<Monkey> monkeys = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.startsWith("Monkey")) {
                    List<Long> items = new ArrayList<>();
                    line = br.readLine().trim(); 
                    String[] itemStrs = line.substring(line.indexOf(":") + 2).split(", ");
                    for (String itemStr : itemStrs) {
                        items.add(Long.parseLong(itemStr));
                    }

                    line = br.readLine().trim();
                    char operation = line.split(" ")[4].charAt(0);
                    long operand;
                    try {
                        operand = Long.parseLong(line.split(" ")[5]);
                    } catch (NumberFormatException e) {
                        operand = -1;
                    }

                    line = br.readLine().trim();
                    int testDivisor = Integer.parseInt(line.split(" ")[3]);

                    line = br.readLine().trim();
                    int trueMonkey = Integer.parseInt(line.split(" ")[5]);

                    line = br.readLine().trim();
                    int falseMonkey = Integer.parseInt(line.split(" ")[5]);

                    monkeys.add(new Monkey(items, operation, operand, testDivisor, trueMonkey, falseMonkey));
                    br.readLine(); 
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            return null;
        }
        return monkeys;
    }
}
