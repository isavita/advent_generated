
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static boolean validatePassword(String policy, String password) {
        String[] parts = policy.split(" ");
        String[] range = parts[0].split("-");
        int min = Integer.parseInt(range[0]);
        int max = Integer.parseInt(range[1]);
        char character = parts[1].charAt(0);
        int count = 0;
        for (int i = 0; i < password.length(); i++) {
            if (password.charAt(i) == character) {
                count++;
            }
        }
        return count >= min && count <= max;
    }

    public static void main(String[] args) {
        int validCount = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            while ((line = reader.readLine()) != null) {
                int i = line.indexOf(":");
                String policy = line.substring(0, i);
                String password = line.substring(i + 2);
                if (validatePassword(policy, password)) {
                    validCount++;
                }
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println(validCount);
    }
}
