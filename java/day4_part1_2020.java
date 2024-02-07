
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<String> passports = new ArrayList<>();
        String passport = "";

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) {
                    passports.add(passport);
                    passport = "";
                } else {
                    passport += " " + line;
                }
            }
            if (!passport.isEmpty()) {
                passports.add(passport);
            }
        } catch (IOException e) {
            System.out.println("Error opening file: " + e.getMessage());
            return;
        }

        int validPassports = 0;
        String[] requiredFields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"};

        for (String p : passports) {
            if (isValid(p, requiredFields)) {
                validPassports++;
            }
        }

        System.out.println(validPassports);
    }

    public static boolean isValid(String passport, String[] requiredFields) {
        for (String field : requiredFields) {
            if (!passport.contains(field + ":")) {
                return false;
            }
        }
        return true;
    }
}
