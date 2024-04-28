import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class MonkeyMath {

    private static class Monkey {
        String name;
        String job;
        Monkey(String name, String job) {
            this.name = name;
            this.job = job;
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        Map<String, Monkey> monkeys = new HashMap<>();
        String line;
        while ((line = br.readLine()) != null) {
            String[] parts = line.split(": ");
            String name = parts[0];
            String job = parts[1];
            monkeys.put(name, new Monkey(name, job));
        }
        br.close();

        Map<String, Long> values = new HashMap<>();
        for (Monkey monkey : monkeys.values()) {
            if (monkey.job.matches("\\d+")) {
                values.put(monkey.name, Long.parseLong(monkey.job));
            }
        }

        while (true) {
            boolean updated = false;
            for (Monkey monkey : monkeys.values()) {
                if (values.containsKey(monkey.name)) {
                    continue;
                }
                String[] parts = monkey.job.split(" ");
                if (parts.length == 1) {
                    if (values.containsKey(parts[0])) {
                        values.put(monkey.name, values.get(parts[0]));
                        updated = true;
                    }
                } else if (parts.length == 3) {
                    if (values.containsKey(parts[0]) && values.containsKey(parts[2])) {
                        long value1 = values.get(parts[0]);
                        long value2 = values.get(parts[2]);
                        switch (parts[1]) {
                            case "+":
                                values.put(monkey.name, value1 + value2);
                                updated = true;
                                break;
                            case "-":
                                values.put(monkey.name, value1 - value2);
                                updated = true;
                                break;
                            case "*":
                                values.put(monkey.name, value1 * value2);
                                updated = true;
                                break;
                            case "/":
                                values.put(monkey.name, value1 / value2);
                                updated = true;
                                break;
                        }
                    }
                }
            }
            if (!updated) {
                break;
            }
        }

        System.out.println(values.get("root"));
    }
}