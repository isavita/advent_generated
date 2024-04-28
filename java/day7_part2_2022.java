import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {
    static class Directory {
        String name;
        long size;
        Map<String, Directory> dirs;
        Map<String, Long> files;
        Directory parent;

        Directory(String name, Directory parent) {
            this.name = name;
            this.parent = parent;
            this.size = 0;
            this.dirs = new HashMap<>();
            this.files = new HashMap<>();
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        Directory root = new Directory("/", null);
        Directory current = root;

        String line;
        while ((line = br.readLine()) != null) {
            String[] parts = line.split(" ");
            if (parts[0].equals("$")) {
                if (parts[1].equals("cd")) {
                    if (parts[2].equals("..")) {
                        current = getParent(current);
                    } else if (parts[2].equals("/")) {
                        current = root;
                    } else {
                        current = current.dirs.get(parts[2]);
                    }
                } else if (parts[1].equals("ls")) {
                    // do nothing
                }
            } else if (parts[0].equals("dir")) {
                current.dirs.put(parts[1], new Directory(parts[1], current));
            } else {
                long size = Long.parseLong(parts[0]);
                current.files.put(parts[1], size);
                current.size += size;
                Directory temp = current;
                while (temp != null) {
                    temp.size += size;
                    temp = temp.parent;
                }
            }
        }

        List<Long> sizes = new ArrayList<>();
        getSizes(root, sizes);
        long sum = sizes.stream().filter(size -> size <= 100000).mapToLong(Long::longValue).sum();
        System.out.println("Part 1: " + sum);

        long totalSize = root.size;
        long requiredSpace = 30000000;
        long availableSpace = 70000000 - totalSize;
        long needSpace = requiredSpace - availableSpace;
        long smallestDirSize = Long.MAX_VALUE;
        for (long size : sizes) {
            if (size >= needSpace && size < smallestDirSize) {
                smallestDirSize = size;
            }
        }
        System.out.println("Part 2: " + smallestDirSize);
    }

    static Directory getParent(Directory dir) {
        return dir.parent;
    }

    static void getSizes(Directory dir, List<Long> sizes) {
        sizes.add(dir.size);
        for (Directory d : dir.dirs.values()) {
            getSizes(d, sizes);
        }
    }
}