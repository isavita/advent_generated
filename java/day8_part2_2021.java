
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JumbledSevenSegment {
    public static void main(String[] args) throws IOException {
        String input = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        System.out.println(jumbledSevenSegment(input));
    }

    private static int jumbledSevenSegment(String input) {
        List<List<String>> parsedInput = new ArrayList<>();
        Pattern pattern = Pattern.compile("([a-g]+)");

        for (String line : input.split("\n")) {
            Matcher matcher = pattern.matcher(line);
            List<String> parts = new ArrayList<>();
            while (matcher.find()) {
                parts.add(alphabetizeString(matcher.group(1)));
            }
            if (parts.size() != 14) throw new IllegalArgumentException("Expected 14 parts");
            parsedInput.add(parts);
        }

        int ans = 0;
        String[] indexToCharacters = new String[10];

        for (List<String> set : parsedInput) {
            List<String> workingSet = set.subList(0, 10);
            List<Integer> killIndices = new ArrayList<>();

            for (int i = 0; i < workingSet.size(); i++) {
                String mapping = workingSet.get(i);
                switch (mapping.length()) {
                    case 2: indexToCharacters[1] = mapping; killIndices.add(i); break;
                    case 4: indexToCharacters[4] = mapping; killIndices.add(i); break;
                    case 3: indexToCharacters[7] = mapping; killIndices.add(i); break;
                    case 7: indexToCharacters[8] = mapping; killIndices.add(i); break;
                }
            }

            workingSet = removeSliceIndices(workingSet, killIndices);
            List<String> zeroThreeOrNine = new ArrayList<>();
            killIndices.clear();

            for (int i = 0; i < workingSet.size(); i++) {
                String mapping = workingSet.get(i);
                if (checkStringOverlap(mapping, indexToCharacters[1])) {
                    zeroThreeOrNine.add(mapping);
                    killIndices.add(i);
                }
            }

            if (zeroThreeOrNine.size() != 3) throw new IllegalArgumentException("Expected 3 matches");

            for (int i = 0; i < zeroThreeOrNine.size(); i++) {
                String maybe039 = zeroThreeOrNine.get(i);
                if (maybe039.length() == 5) {
                    indexToCharacters[3] = maybe039;
                    zeroThreeOrNine.remove(i);
                    break;
                }
            }

            for (int i = 0; i < zeroThreeOrNine.size(); i++) {
                String maybe09 = zeroThreeOrNine.get(i);
                if (checkStringOverlap(maybe09, indexToCharacters[4])) {
                    indexToCharacters[9] = maybe09;
                    zeroThreeOrNine.remove(i);
                }
            }
            indexToCharacters[0] = zeroThreeOrNine.get(0);
            workingSet = removeSliceIndices(workingSet, killIndices);

            if (workingSet.size() != 3) throw new IllegalArgumentException("Expected length of 3");

            for (int i = 0; i < workingSet.size(); i++) {
                String mapping = workingSet.get(i);
                if (mapping.length() == 6) {
                    indexToCharacters[6] = mapping;
                    workingSet.remove(i);
                    break;
                }
            }

            for (int i = 0; i < workingSet.size(); i++) {
                String mapping = workingSet.get(i);
                if (checkStringOverlap(indexToCharacters[9], mapping)) {
                    indexToCharacters[5] = mapping;
                    workingSet.remove(i);
                    break;
                }
            }

            if (workingSet.size() != 1) throw new IllegalArgumentException("Expected length of 1");

            indexToCharacters[2] = workingSet.get(0);
            int num = 0;

            for (String out : set.subList(10, set.size())) {
                for (int i = 0; i < indexToCharacters.length; i++) {
                    if (out.equals(indexToCharacters[i])) {
                        num = num * 10 + i;
                    }
                }
            }
            ans += num;
        }
        return ans;
    }

    private static List<String> removeSliceIndices(List<String> list, List<Integer> indices) {
        List<String> result = new ArrayList<>();
        Set<Integer> indexSet = new HashSet<>(indices);
        for (int i = 0; i < list.size(); i++) {
            if (!indexSet.contains(i)) {
                result.add(list.get(i));
            }
        }
        return result;
    }

    private static boolean checkStringOverlap(String larger, String smaller) {
        return smaller.chars().allMatch(c -> larger.indexOf(c) >= 0);
    }

    private static String alphabetizeString(String input) {
        char[] chars = input.toCharArray();
        Arrays.sort(chars);
        return new String(chars);
    }
}
