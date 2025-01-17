
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class MonkeyMarket {

    private static final int MODULUS = 16777216;
    private static final int NUM_SECRETS = 2000;

    public static void main(String[] args) {
        long sumOf2000thSecrets = 0;

        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                long secret = Long.parseLong(line);
                sumOf2000thSecrets += simulateSecrets(secret);
            }
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }

        System.out.println(sumOf2000thSecrets);
    }

    private static long simulateSecrets(long initialSecret) {
        long secret = initialSecret;
        for (int i = 0; i < NUM_SECRETS; i++) {
            secret = mixAndPrune(secret * 64, secret);
            secret = mixAndPrune((long) Math.floor(secret / 32.0), secret);
            secret = mixAndPrune(secret * 2048, secret);
        }
        return secret;
    }

    private static long mixAndPrune(long value, long secret) {
        return (value ^ secret) % MODULUS;
    }
}
