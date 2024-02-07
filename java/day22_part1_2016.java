
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {

    static class Node {
        int used;
        int avail;

        Node(int used, int avail) {
            this.used = used;
            this.avail = avail;
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        ArrayList<Node> nodes = readNodes("input.txt");
        int viablePairs = countViablePairs(nodes);
        System.out.println(viablePairs);
    }

    public static ArrayList<Node> readNodes(String filename) throws FileNotFoundException {
        ArrayList<Node> nodes = new ArrayList<>();
        File file = new File(filename);
        Scanner scanner = new Scanner(file);
        Pattern nodePattern = Pattern.compile("node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%");

        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            Matcher matcher = nodePattern.matcher(line);
            if (matcher.find()) {
                int used = Integer.parseInt(matcher.group(1));
                int avail = Integer.parseInt(matcher.group(2));
                nodes.add(new Node(used, avail));
            }
        }
        return nodes;
    }

    public static int countViablePairs(ArrayList<Node> nodes) {
        int count = 0;
        for (int i = 0; i < nodes.size(); i++) {
            for (int j = 0; j < nodes.size(); j++) {
                if (i != j && nodes.get(i).used > 0 && nodes.get(i).used <= nodes.get(j).avail) {
                    count++;
                }
            }
        }
        return count;
    }
}
