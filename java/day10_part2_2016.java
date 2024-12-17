
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    static class Bot {
        String lowTo;
        String highTo;
        List<Integer> chips = new ArrayList<>();
    }

    public static void main(String[] args) {
        Map<String, Bot> bots = new HashMap<>();
        Map<String, Integer> outputs = new HashMap<>();
        Pattern valueRegex = Pattern.compile("value (\\d+) goes to (bot \\d+)");
        Pattern givesRegex = Pattern.compile("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)");

        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                Matcher valueMatcher = valueRegex.matcher(line);
                if (valueMatcher.matches()) {
                    int value = Integer.parseInt(valueMatcher.group(1));
                    String botID = valueMatcher.group(2);
                    bots.computeIfAbsent(botID, k -> new Bot()).chips.add(value);
                } else {
                    Matcher givesMatcher = givesRegex.matcher(line);
                    if (givesMatcher.matches()) {
                        String botID = givesMatcher.group(1);
                        String lowTo = givesMatcher.group(2);
                        String highTo = givesMatcher.group(3);
                        Bot bot = bots.computeIfAbsent(botID, k -> new Bot());
                        bot.lowTo = lowTo;
                        bot.highTo = highTo;
                    }
                }
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return;
        }

        boolean action;
        do {
            action = false;
            for (Map.Entry<String, Bot> entry : bots.entrySet()) {
                Bot b = entry.getValue();
                if (b.chips.size() == 2) {
                    action = true;
                    int low = Math.min(b.chips.get(0), b.chips.get(1));
                    int high = Math.max(b.chips.get(0), b.chips.get(1));
                    b.chips.clear();
                    giveChip(bots, outputs, b.lowTo, low);
                    giveChip(bots, outputs, b.highTo, high);
                }
            }
        } while (action);

        int result = outputs.getOrDefault("output 0", 1) * outputs.getOrDefault("output 1", 1) * outputs.getOrDefault("output 2", 1);
        System.out.println(result);
    }

    static void giveChip(Map<String, Bot> bots, Map<String, Integer> outputs, String target, int value) {
        if (target.startsWith("bot")) {
            bots.computeIfAbsent(target, k -> new Bot()).chips.add(value);
        } else if (target.startsWith("output")) {
            outputs.put(target, value);
        }
    }
}
