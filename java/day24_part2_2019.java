
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {

    private static final int Side = 5;
    private static final int Square = Side * Side;

    public static boolean[] parse() {
        boolean[] res = new boolean[Square];

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int row = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (int col = 0; col < Side; col++) {
                    if (line.charAt(col) == '#') {
                        res[row * Side + col] = true;
                    } else {
                        res[row * Side + col] = false;
                    }
                }
                row++;
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        return res;
    }

    public static void main(String[] args) {
        boolean[] input = parse();

        java.util.Map<Integer, boolean[]> space = new java.util.HashMap<>();
        space.put(0, input);

        for (int i = 0; i < 200; i++) {
            space = next2(space);
        }

        int count = 0;
        for (boolean[] grid : space.values()) {
            for (int i = 0; i < Square; i++) {
                if (grid[i]) {
                    count++;
                }
            }
        }
        System.out.println(count);
    }

    public static java.util.Map<Integer, boolean[]> next2(java.util.Map<Integer, boolean[]> space) {
        java.util.Map<Integer, boolean[]> newSpace = new java.util.HashMap<>();

        int[] minMaxLevel = minMaxLevel(space);

        int minLevel = minMaxLevel[0];
        int maxLevel = minMaxLevel[1];

        for (int level = minLevel - 1; level <= maxLevel + 1; level++) {
            boolean[] grid = new boolean[Square];

            for (int cell = 0; cell < Square; cell++) {

                if (cell == 12) {
                    continue;
                }

                int row = cell / Side;
                int col = cell % Side;
                int neighbours = 0;

                if (row == 0) {
                    if (infested(space, level - 1, 7)) {
                        neighbours++;
                    }
                }

                if (col == 0) {
                    if (infested(space, level - 1, 11)) {
                        neighbours++;
                    }
                }

                if (col == 4) {
                    if (infested(space, level - 1, 13)) {
                        neighbours++;
                    }
                }

                if (row == 4) {
                    if (infested(space, level - 1, 17)) {
                        neighbours++;
                    }
                }

                if (cell == 7) {
                    for (int i = 0; i < Side; i++) {
                        if (infested(space, level + 1, i)) {
                            neighbours++;
                        }
                    }
                }

                if (cell == 11) {
                    for (int i = 0; i < Side; i++) {
                        if (infested(space, level + 1, 5 * i)) {
                            neighbours++;
                        }
                    }
                }

                if (cell == 13) {
                    for (int i = 0; i < Side; i++) {
                        if (infested(space, level + 1, 5 * i + Side - 1)) {
                            neighbours++;
                        }
                    }
                }

                if (cell == 17) {
                    for (int i = 0; i < Side; i++) {
                        if (infested(space, level + 1, (Side - 1) * Side + i)) {
                            neighbours++;
                        }
                    }
                }

                if (row > 0 && cell != 17) {
                    if (infested(space, level, cell - Side)) {
                        neighbours++;
                    }
                }

                if (col > 0 && cell != 13) {
                    if (infested(space, level, cell - 1)) {
                        neighbours++;
                    }
                }

                if (col < Side - 1 && cell != 11) {
                    if (infested(space, level, cell + 1)) {
                        neighbours++;
                    }
                }

                if (row < Side - 1 && cell != 7) {
                    if (infested(space, level, cell + Side)) {
                        neighbours++;
                    }
                }

                if (infested(space, level, cell) && neighbours != 1) {
                    grid[cell] = false;
                    continue;
                }

                if (!infested(space, level, cell) && (neighbours == 1 || neighbours == 2)) {
                    grid[cell] = true;
                    continue;
                }

                grid[cell] = infested(space, level, cell);
            }
            newSpace.put(level, grid);
        }

        clean(newSpace);

        return newSpace;
    }

    public static void clean(java.util.Map<Integer, boolean[]> space) {
        int[] minMaxLevel = minMaxLevel(space);

        int min = minMaxLevel[0];
        int max = minMaxLevel[1];

        int countMin = 0;
        int countMax = 0;
        for (int cell = 0; cell < Square; cell++) {
            if (space.get(min)[cell]) {
                countMin++;
            }
            if (space.get(max)[cell]) {
                countMax++;
            }
        }
        if (countMin == 0) {
            space.remove(min);
        }
        if (countMax == 0) {
            space.remove(max);
        }
    }

    public static boolean infested(java.util.Map<Integer, boolean[]> space, int level, int cell) {
        if (space.get(level) == null) {
            return false;
        }
        return space.get(level)[cell];
    }

    public static int[] minMaxLevel(java.util.Map<Integer, boolean[]> space) {
        int min = 999999;
        int max = -999999;
        for (int level : space.keySet()) {
            if (level < min) {
                min = level;
            }
            if (level > max) {
                max = level;
            }
        }
        return new int[]{min, max};
    }
}
