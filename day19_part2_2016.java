
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            String input = readFile("input.txt");
            int ans = elephant(input);
            System.out.println(ans);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static class LLNode {
        int elfNum;
        int presents;
        LLNode next;

        LLNode(int elfNum, int presents) {
            this.elfNum = elfNum;
            this.presents = presents;
        }
    }

    private static int elephant(String input) {
        int startingElves = Integer.parseInt(input);
        LLNode root = new LLNode(1, 1);
        LLNode iter = root;
        for (int i = 2; i <= startingElves; i++) {
            iter.next = new LLNode(i, 1);
            iter = iter.next;
        }
        iter.next = root;

        boolean isOddLength = startingElves % 2 == 1;
        LLNode beforeAcross = root;
        for (int i = 0; i < startingElves / 2 - 1; i++) {
            beforeAcross = beforeAcross.next;
        }

        while (root.next != root) {
            root.presents += beforeAcross.next.presents;
            beforeAcross.next = beforeAcross.next.next;

            if (isOddLength) {
                beforeAcross = beforeAcross.next;
            }
            isOddLength = !isOddLength;
            root = root.next;
        }

        return root.elfNum;
    }

    private static String readFile(String fileName) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(fileName));
        String line = reader.readLine();
        reader.close();
        return line;
    }
}
