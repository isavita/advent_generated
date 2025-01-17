
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FileSystem {

    static class Node {
        String name;
        int size;
        Node parent;
        Map<String, Node> children;

        public Node(String name, Node parent) {
            this.name = name;
            this.parent = parent;
            this.size = 0;
            this.children = new HashMap<>();
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            Node root = new Node("/", null);
            Node current = root;
            String line;

            while ((line = reader.readLine()) != null) {
                if (line.startsWith("$ cd")) {
                    String dirName = line.substring(5);
                    if (dirName.equals("/")) {
                        current = root;
                    } else if (dirName.equals("..")) {
                        current = current.parent;
                    } else {
                        current = current.children.get(dirName);
                    }
                } else if (!line.startsWith("$ ls")) {
                    String[] parts = line.split(" ");
                    if (parts[0].equals("dir")) {
                        current.children.put(parts[1], new Node(parts[1], current));
                    } else {
                        int fileSize = Integer.parseInt(parts[0]);
                        current.children.put(parts[1], new Node(parts[1], current));
                        
                        Node temp = current;
                        while(temp != null){
                            temp.size += fileSize;
                            temp = temp.parent;
                        }
                    }
                }
            }

            List<Node> directories = new ArrayList<>();
            collectDirectories(root, directories);

            long sum = directories.stream()
                    .filter(dir -> dir.size <= 100000)
                    .mapToLong(dir -> dir.size)
                    .sum();

            System.out.println(sum);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void collectDirectories(Node node, List<Node> directories) {
        if (!node.children.isEmpty()) {
            directories.add(node);
            for (Node child : node.children.values()) {
                collectDirectories(child, directories);
            }
        }
    }
}
