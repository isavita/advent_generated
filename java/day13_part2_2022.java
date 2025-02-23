
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class DistressSignal {

    public static void main(String[] args) throws IOException {
        String filePath = "input.txt";
        part1(filePath);
        part2(filePath);

    }

    private static void part1(String filePath) throws IOException{
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line1, line2;
            int pairIndex = 1;
            int sumOfIndices = 0;

            while ((line1 = reader.readLine()) != null && (line2 = reader.readLine()) != null) {
                if (comparePackets(parsePacket(line1), parsePacket(line2)) < 0) {
                    sumOfIndices += pairIndex;
                }
                pairIndex++;
                reader.readLine(); // Consume the blank line
            }
            System.out.println("Part 1 - Sum of indices of correctly ordered pairs: " + sumOfIndices);
        }
    }

    private static void part2(String filePath) throws IOException{
        List<Object> allPackets = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    allPackets.add(parsePacket(line));
                }
            }
        }

        // Add divider packets
        Object divider1 = parsePacket("[[2]]");
        Object divider2 = parsePacket("[[6]]");
        allPackets.add(divider1);
        allPackets.add(divider2);

        // Sort all packets
        Collections.sort(allPackets, DistressSignal::comparePackets);

        // Find indices of divider packets
        int index1 = allPackets.indexOf(divider1) + 1;
        int index2 = allPackets.indexOf(divider2) + 1;

        System.out.println("Part 2 - Decoder key: " + (index1 * index2));
    }
    private static Object parsePacket(String s) {
        return parsePacketRecursive(s, 0).packet;
    }

    private static ParseResult parsePacketRecursive(String s, int index) {
        List<Object> currentList = new ArrayList<>();
        StringBuilder currentNumber = new StringBuilder();

        while (index < s.length()) {
            char c = s.charAt(index);

            if (c == '[') {
                ParseResult innerResult = parsePacketRecursive(s, index + 1);
                currentList.add(innerResult.packet);
                index = innerResult.nextIndex;
            } else if (c == ']') {
                if (currentNumber.length() > 0) {
                    currentList.add(Integer.parseInt(currentNumber.toString()));
                    currentNumber.setLength(0); // Clear the StringBuilder
                }
                return new ParseResult(currentList, index + 1);
            } else if (c == ',') {
                if (currentNumber.length() > 0) {
                    currentList.add(Integer.parseInt(currentNumber.toString()));
                    currentNumber.setLength(0); // Clear the StringBuilder
                }
                index++;
            } else {
                currentNumber.append(c);
                index++;
            }
        }
          if (currentNumber.length() > 0) {
              currentList.add(Integer.parseInt(currentNumber.toString()));
          }
        return new ParseResult(currentList,index);
    }

    private static class ParseResult {
        Object packet;
        int nextIndex;

        ParseResult(Object packet, int nextIndex) {
            this.packet = packet;
            this.nextIndex = nextIndex;
        }
    }

    private static int comparePackets(Object left, Object right) {
        if (left instanceof Integer && right instanceof Integer) {
            return ((Integer) left).compareTo((Integer) right);
        } else if (left instanceof List && right instanceof List) {
            List<?> leftList = (List<?>) left;
            List<?> rightList = (List<?>) right;
            int minSize = Math.min(leftList.size(), rightList.size());
            for (int i = 0; i < minSize; i++) {
                int comparison = comparePackets(leftList.get(i), rightList.get(i));
                if (comparison != 0) {
                    return comparison;
                }
            }
            return Integer.compare(leftList.size(), rightList.size());
        } else if (left instanceof Integer) {
            return comparePackets(Collections.singletonList(left), right);
        } else {
            return comparePackets(left, Collections.singletonList(right));
        }
    }
}
