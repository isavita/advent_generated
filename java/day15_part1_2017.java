
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));

            long genAStart = Long.parseLong(reader.readLine());
            long genBStart = Long.parseLong(reader.readLine());

            long genAFactor = 16807;
            long genBFactor = 48271;
            long modulus = 2147483647;

            long genA = genAStart;
            long genB = genBStart;
            int matches = 0;

            for (int i = 0; i < 40000000; i++) {
                genA = (genA * genAFactor) % modulus;
                genB = (genB * genBFactor) % modulus;

                if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
                    matches++;
                }
            }

            System.out.println(matches);

            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }
}
