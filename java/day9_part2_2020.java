import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class XMAS {
    public static void main(String[] args) throws IOException {
        List<Long> numbers = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                numbers.add(Long.parseLong(line));
            }
        }

        int preambleSize = 25;
        long invalidNumber = findInvalidNumber(numbers, preambleSize);
        System.out.println("Part 1: " + invalidNumber);

        long weakness = findEncryptionWeakness(numbers, invalidNumber);
        System.out.println("Part 2: " + weakness);
    }

    private static long findInvalidNumber(List<Long> numbers, int preambleSize) {
        for (int i = preambleSize; i < numbers.size(); i++) {
            if (!isValidNumber(numbers, i, preambleSize)) {
                return numbers.get(i);
            }
        }
        return -1;
    }

    private static boolean isValidNumber(List<Long> numbers, int index, int preambleSize) {
        long target = numbers.get(index);
        Set<Long> preamble = new HashSet<>();
        for (int i = index - preambleSize; i < index; i++) {
            preamble.add(numbers.get(i));
        }
        for (long num : preamble) {
            long complement = target - num;
            if (complement != num && preamble.contains(complement)) {
                return true;
            }
        }
        return false;
    }

    private static long findEncryptionWeakness(List<Long> numbers, long target) {
        for (int i = 0; i < numbers.size(); i++) {
            long sum = 0;
            for (int j = i; j < numbers.size(); j++) {
                sum += numbers.get(j);
                if (sum == target) {
                    List<Long> contiguousSet = numbers.subList(i, j + 1);
                    Collections.sort(contiguousSet);
                    return contiguousSet.get(0) + contiguousSet.get(contiguousSet.size() - 1);
                }
                if (sum > target) {
                    break;
                }
            }
        }
        return -1;
    }
}