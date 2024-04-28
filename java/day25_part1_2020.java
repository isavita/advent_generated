import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    private static final int MODULO = 20201227;

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            long cardPublicKey = Long.parseLong(br.readLine());
            long doorPublicKey = Long.parseLong(br.readLine());

            long cardLoopSize = findLoopSize(7, cardPublicKey);
            long doorLoopSize = findLoopSize(7, doorPublicKey);

            long encryptionKey = transformSubjectNumber(doorPublicKey, cardLoopSize);

            System.out.println("Encryption key: " + encryptionKey);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static long findLoopSize(long subjectNumber, long publicKey) {
        long value = 1;
        long loopSize = 0;

        while (true) {
            value = (value * subjectNumber) % MODULO;
            loopSize++;

            if (value == publicKey) {
                break;
            }
        }

        return loopSize;
    }

    private static long transformSubjectNumber(long subjectNumber, long loopSize) {
        long value = 1;

        for (int i = 0; i < loopSize; i++) {
            value = (value * subjectNumber) % MODULO;
        }

        return value;
    }
}