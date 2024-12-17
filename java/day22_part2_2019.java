
import java.io.File;
import java.math.BigInteger;
import java.util.Scanner;

public class Main {

    static final BigInteger SIZE = new BigInteger("119315717514047");
    static final BigInteger ITER = new BigInteger("101741582076661");

    public static void main(String[] args) throws Exception {
        BigInteger offset = BigInteger.ZERO;
        BigInteger increment = BigInteger.ONE;

        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);

        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.equals("deal into new stack")) {
                increment = increment.negate();
                offset = offset.add(increment);
            } else if (line.startsWith("cut")) {
                String[] parts = line.split(" ");
                int n = Integer.parseInt(parts[1]);
                offset = offset.add(BigInteger.valueOf(n).multiply(increment));
            } else if (line.startsWith("deal with increment")) {
                String[] parts = line.split(" ");
                int n = Integer.parseInt(parts[parts.length - 1]);
                increment = increment.multiply(BigInteger.valueOf(n).modInverse(SIZE));
            }
        }
        scanner.close();

        BigInteger finalIncr = increment.modPow(ITER, SIZE);
        BigInteger finalOffs = increment.modPow(ITER, SIZE).subtract(BigInteger.ONE);
        finalOffs = finalOffs.multiply(increment.subtract(BigInteger.ONE).modInverse(SIZE)).multiply(offset);

        BigInteger answer = BigInteger.valueOf(2020).multiply(finalIncr).add(finalOffs).mod(SIZE);
        System.out.println(answer);
    }
}
