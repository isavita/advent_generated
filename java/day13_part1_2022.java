
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class DistressSignal {

    public static void main(String[] args) {
        int sumOfIndices = 0;
        int pairIndex = 1;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line1;
            while ((line1 = br.readLine()) != null) {
                String line2 = br.readLine();
                if (line2 == null) {
                    break; // Should not happen with well-formed input, but handle it anyway.
                }

                Object packet1 = parsePacket(line1);
                Object packet2 = parsePacket(line2);

                if (comparePackets(packet1, packet2) < 0) {
                    sumOfIndices += pairIndex;
                }

                pairIndex++;
                br.readLine(); // Consume the blank line separating pairs
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }

        System.out.println(sumOfIndices);
    }

    // Parses a packet string into a nested List structure.
    private static Object parsePacket(String line) {
        return parseList(line, 0).get(0);
    }
    
    private static List<Object> parseList(String line, int start) {
        List<Object> list = new ArrayList<>();
        int i = start + 1;
        while (i < line.length()) {
            char c = line.charAt(i);
            if (c == '[') {
                List<Object> sublist = parseList(line, i);
                list.add(sublist.get(0));                
                i = (int) sublist.get(1);
            } else if (c == ']') {
                List<Object> returnList = new ArrayList();
                returnList.add(list);
                returnList.add(i+1);          
                return returnList;                
            } else if (Character.isDigit(c)) {
                int j = i;
                while (j < line.length() && Character.isDigit(line.charAt(j))) {
                    j++;
                }
                list.add(Integer.parseInt(line.substring(i, j)));
                i = j;
            } else {
                i++; // Skip commas
            }
        }
        return null; // Should never reach here for well-formed input
    }
    

    // Compares two packets recursively.  Returns -1 if left < right, 1 if left > right, 0 if equal.
    @SuppressWarnings("unchecked")
    private static int comparePackets(Object left, Object right) {
        if (left instanceof Integer && right instanceof Integer) {
            return Integer.compare((Integer) left, (Integer) right);
        } else if (left instanceof List && right instanceof List) {
            List<Object> leftList = (List<Object>) left;
            List<Object> rightList = (List<Object>) right;
            int i = 0;
            while (i < leftList.size() && i < rightList.size()) {
                int comparison = comparePackets(leftList.get(i), rightList.get(i));
                if (comparison != 0) {
                    return comparison;
                }
                i++;
            }
            return Integer.compare(leftList.size(), rightList.size());
        } else if (left instanceof Integer) {
            List<Object> newList = new ArrayList<>();
            newList.add(left);
            return comparePackets(newList, right);
        } else {
            List<Object> newList = new ArrayList<>();
            newList.add(right);
            return comparePackets(left, newList);
        }
    }
}
