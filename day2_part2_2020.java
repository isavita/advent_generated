
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static boolean validatePassword(String policy, String password) {
        int min = 0, max = 0;
        char character = 0;
        String[] parts = policy.split("-");
        min = Integer.parseInt(parts[0]);
        max = Integer.parseInt(parts[1].substring(0, parts[1].indexOf(" ")));
        character = parts[1].charAt(parts[1].length() - 1);

        return (password.charAt(min - 1) == character) != (password.charAt(max - 1) == character);
    }

    public static void main(String[] args) {
        int validCount = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                int i = line.indexOf(":");
                if (i != -1) {
                    String policy = line.substring(0, i);
                    String password = line.substring(i + 2);
                    if (validatePassword(policy, password)) {
                        validCount++;
                    }
                }
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println(validCount);
    }
}
