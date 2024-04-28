import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class Main {
    public static void main(String[] args) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String passport = "";
            int validPassports = 0;

            String line;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) {
                    if (isValidPassport(passport)) {
                        validPassports++;
                    }
                    passport = "";
                } else {
                    passport += " " + line;
                }
            }
            if (!passport.isEmpty()) {
                if (isValidPassport(passport)) {
                    validPassports++;
                }
            }

            System.out.println(validPassports);
        }
    }

    public static boolean isValidPassport(String passport) {
        String[] fields = passport.split("\\s+");
        Map<String, String> fieldMap = new HashMap<>();
        for (String field : fields) {
            String[] parts = field.split(":");
            if (parts.length == 2) {
                fieldMap.put(parts[0], parts[1]);
            }
        }

        return validateByr(fieldMap.get("byr")) &&
                validateIyr(fieldMap.get("iyr")) &&
                validateEyr(fieldMap.get("eyr")) &&
                validateHgt(fieldMap.get("hgt")) &&
                validateHcl(fieldMap.get("hcl")) &&
                validateEcl(fieldMap.get("ecl")) &&
                validatePid(fieldMap.get("pid"));
    }

    public static boolean validateByr(String value) {
        return validateYear(value, 1920, 2002);
    }

    public static boolean validateIyr(String value) {
        return validateYear(value, 2010, 2020);
    }

    public static boolean validateEyr(String value) {
        return validateYear(value, 2020, 2030);
    }

    public static boolean validateYear(String value, int min, int max) {
        if (value == null) {
            return false;
        }
        try {
            int year = Integer.parseInt(value);
            return year >= min && year <= max;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean validateHgt(String value) {
        if (value == null) {
            return false;
        }
        if (value.endsWith("cm")) {
            try {
                int hgt = Integer.parseInt(value.substring(0, value.length() - 2));
                return hgt >= 150 && hgt <= 193;
            } catch (NumberFormatException e) {
                return false;
            }
        } else if (value.endsWith("in")) {
            try {
                int hgt = Integer.parseInt(value.substring(0, value.length() - 2));
                return hgt >= 59 && hgt <= 76;
            } catch (NumberFormatException e) {
                return false;
            }
        }
        return false;
    }

    public static boolean validateHcl(String value) {
        if (value == null) {
            return false;
        }
        return Pattern.matches("#[0-9a-f]{6}", value);
    }

    public static boolean validateEcl(String value) {
        if (value == null) {
            return false;
        }
        String[] validEcl = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"};
        for (String ecl : validEcl) {
            if (ecl.equals(value)) {
                return true;
            }
        }
        return false;
    }

    public static boolean validatePid(String value) {
        if (value == null) {
            return false;
        }
        return Pattern.matches("\\d{9}", value);
    }
}