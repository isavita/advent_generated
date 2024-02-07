
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            StringBuilder sb = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                sb.append(line).append("\n");
            }
            reader.close();
            String input = sb.toString();
            int res = auntSue(input);
            System.out.println(res);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static Map<String, Integer> targetSue = new HashMap<String, Integer>() {{
        put("children", 3);
        put("cats", 7);
        put("samoyeds", 2);
        put("pomeranians", 3);
        put("akitas", 0);
        put("vizslas", 0);
        put("goldfish", 5);
        put("trees", 3);
        put("cars", 2);
        put("perfumes", 1);
    }};

    public static int auntSue(String input) {
        for (String line : input.split("\n")) {
            String[] parts = line.split(" ");
            int sueNum = Integer.parseInt(parts[1].split(":")[0]);
            String thing1 = parts[2].split(":")[0];
            int amount1 = Integer.parseInt(parts[3].split(",")[0]);
            String thing2 = parts[4].split(":")[0];
            int amount2 = Integer.parseInt(parts[5].split(",")[0]);
            String thing3 = parts[6].split(":")[0];
            int amount3 = Integer.parseInt(parts[7]);

            Map<String, Integer> readingsMap = new HashMap<>();
            readingsMap.put(thing1, amount1);
            readingsMap.put(thing2, amount2);
            readingsMap.put(thing3, amount3);

            boolean allRulesMatched = true;
            for (String check : new String[]{"cats", "trees"}) {
                if (readingsMap.containsKey(check)) {
                    int scanCount = readingsMap.get(check);
                    if (scanCount <= targetSue.get(check)) {
                        allRulesMatched = false;
                    }
                    readingsMap.remove(check);
                }
            }
            for (String check : new String[]{"pomeranians", "goldfish"}) {
                if (readingsMap.containsKey(check)) {
                    int scanCount = readingsMap.get(check);
                    if (scanCount >= targetSue.get(check)) {
                        allRulesMatched = false;
                    }
                    readingsMap.remove(check);
                }
            }
            for (Map.Entry<String, Integer> entry : readingsMap.entrySet()) {
                String thing = entry.getKey();
                int amount = entry.getValue();
                if (targetSue.get(thing) != amount) {
                    allRulesMatched = false;
                }
            }
            if (allRulesMatched) {
                return sueNum;
            }
        }
        throw new RuntimeException("Expect return from loop");
    }
}
