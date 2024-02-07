
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {

    private static final int Size = 10007;

    public static void main(String[] args) {
        int[] deck = new int[Size];
        for (int i = 0; i < Size; i++) {
            deck[i] = i;
        }

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();

                if (line.equals("deal into new stack")) {
                    deck = dealIntoNewStack(deck);
                    continue;
                }

                if (line.startsWith("cut")) {
                    int n = Integer.parseInt(line.split(" ")[1]);
                    deck = cutN(deck, n);
                    continue;
                }

                if (line.startsWith("deal with increment")) {
                    int n = Integer.parseInt(line.split(" ")[line.split(" ").length - 1]);
                    deck = dealWithIncrement(deck, n);
                    continue;
                }
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        System.out.println(find2019(deck));
    }

    private static int[] dealIntoNewStack(int[] deck) {
        for (int i = 0; i < Size / 2; i++) {
            int temp = deck[i];
            deck[i] = deck[Size - i - 1];
            deck[Size - i - 1] = temp;
        }
        return deck;
    }

    private static int[] cutN(int[] deck, int n) {
        int[] newDeck = new int[Size];
        if (n >= 0) {
            System.arraycopy(deck, n, newDeck, 0, Size - n);
            System.arraycopy(deck, 0, newDeck, Size - n, n);
        } else {
            System.arraycopy(deck, Size + n, newDeck, 0, -n);
            System.arraycopy(deck, 0, newDeck, -n, Size + n);
        }
        return newDeck;
    }

    private static int[] dealWithIncrement(int[] deck, int n) {
        int[] newDeck = new int[Size];
        for (int i = 0; i < Size; i++) {
            newDeck[(i * n) % Size] = deck[i];
        }
        return newDeck;
    }

    private static int find2019(int[] deck) {
        for (int i = 0; i < Size; i++) {
            if (deck[i] == 2019) {
                return i;
            }
        }
        return -1;
    }
}
