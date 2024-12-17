
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
        Pattern valueRegex = Pattern.compile("value (\\d+) goes to (bot \\d+)");
        Pattern givesRegex = Pattern.compile("(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)");

        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                Matcher valueMatcher = valueRegex.matcher(line);
                if (valueMatcher.find()) {
                    int value = Integer.parseInt(valueMatcher.group(1));
                    String botID = valueMatcher.group(2);
                    bots.computeIfAbsent(botID, k -> new Bot()).chips.add(value);
                } else {
                    Matcher givesMatcher = givesRegex.matcher(line);
                    if (givesMatcher.find()) {
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

        while (true) {
            boolean action = false;
            for (Map.Entry<String, Bot> entry : bots.entrySet()) {
                String botID = entry.getKey();
                Bot b = entry.getValue();
                if (b.chips.size() == 2) {
                    action = true;
                    int low = Math.min(b.chips.get(0), b.chips.get(1));
                    int high = Math.max(b.chips.get(0), b.chips.get(1));
                    if (low == 17 && high == 61) {
                        System.out.println(botID);
                        return;
                    }
                    b.chips.clear();
                    giveChip(bots, b.lowTo, low);
                    giveChip(bots, b.highTo, high);
                }
            }
            if (!action) {
                break;
            }
        }
    }

    static void giveChip(Map<String, Bot> bots, String target, int value) {
        bots.computeIfAbsent(target, k -> new Bot()).chips.add(value);
    }
}
