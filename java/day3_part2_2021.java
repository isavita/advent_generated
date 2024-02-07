
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static String filterValues(List<String> values, Criteria criteria) {
        for (int i = 0; i < values.get(0).length(); i++) {
            int zeros = 0, ones = 0;
            for (String val : values) {
                if (val.charAt(i) == '0') {
                    zeros++;
                } else {
                    ones++;
                }
            }
            char keep = criteria.choose(zeros, ones);
            values = filterByBit(values, i, keep);
            if (values.size() == 1) {
                break;
            }
        }
        return values.get(0);
    }

    public static List<String> filterByBit(List<String> values, int bitIndex, char keep) {
        List<String> filtered = new ArrayList<>();
        for (String val : values) {
            if (val.charAt(bitIndex) == keep) {
                filtered.add(val);
            }
        }
        return filtered;
    }

    public static void main(String[] args) {
        List<String> values = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                values.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        String oxygenGeneratorRating = filterValues(values, (zeros, ones) -> {
            if (zeros > ones) {
                return '0';
            } else {
                return '1';
            }
        });
        long oxygenGeneratorRatingInt = Long.parseLong(oxygenGeneratorRating, 2);

        String co2ScrubberRating = filterValues(values, (zeros, ones) -> {
            if (zeros <= ones) {
                return '0';
            } else {
                return '1';
            }
        });
        long co2ScrubberRatingInt = Long.parseLong(co2ScrubberRating, 2);

        System.out.println(oxygenGeneratorRatingInt * co2ScrubberRatingInt);
    }

    interface Criteria {
        char choose(int zeros, int ones);
    }
}
