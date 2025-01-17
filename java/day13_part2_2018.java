
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class MineCartMadness {

    public static void main(String[] args) {
        try {
            List<String> lines = readInput("input.txt");
            char[][] grid = new char[lines.size()][lines.get(0).length()];
            List<Cart> carts = new ArrayList<>();

            for (int y = 0; y < lines.size(); y++) {
                String line = lines.get(y);
                for (int x = 0; x < line.length(); x++) {
                    char c = line.charAt(x);
                    grid[y][x] = c;
                    if (c == '^' || c == 'v' || c == '<' || c == '>') {
                        carts.add(new Cart(x, y, c));
                        grid[y][x] = (c == '^' || c == 'v') ? '|' : '-';
                    }
                }
            }

            // Part 1
            // findFirstCollision(grid, new ArrayList<>(carts));

            // Part 2
            findLastCartLocation(grid, carts);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void findFirstCollision(char[][] grid, List<Cart> carts) {
        while (true) {
            Collections.sort(carts, Comparator.comparingInt((Cart c) -> c.y * grid[0].length + c.x));
            for (Cart cart : carts) {
                cart.move(grid);
                for (Cart other : carts) {
                    if (cart != other && cart.x == other.x && cart.y == other.y) {
                        System.out.println("First collision at: " + cart.x + "," + cart.y);
                        return;
                    }
                }
            }
        }
    }

    private static void findLastCartLocation(char[][] grid, List<Cart> carts) {
        while (carts.size() > 1) {
            Collections.sort(carts, Comparator.comparingInt((Cart c) -> c.y * grid[0].length + c.x));
            List<Cart> toRemove = new ArrayList<>();
            for (int i = 0; i < carts.size(); i++) {
                Cart cart = carts.get(i);
                if (toRemove.contains(cart)) continue;
                cart.move(grid);
                for (int j = 0; j < carts.size(); j++) {
                    Cart other = carts.get(j);
                    if (cart != other && cart.x == other.x && cart.y == other.y) {
                        toRemove.add(cart);
                        toRemove.add(other);
                        break;
                    }
                }
            }
            carts.removeAll(toRemove);
        }
        if (carts.size() == 1) {
            Cart lastCart = carts.get(0);
            System.out.println("Last cart location: " + lastCart.x + "," + lastCart.y);
        }
    }

    private static List<String> readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }

    static class Cart {
        int x;
        int y;
        char direction;
        int intersectionCount;

        public Cart(int x, int y, char direction) {
            this.x = x;
            this.y = y;
            this.direction = direction;
            this.intersectionCount = 0;
        }

        public void move(char[][] grid) {
            if (direction == '^') {
                y--;
            } else if (direction == 'v') {
                y++;
            } else if (direction == '<') {
                x--;
            } else if (direction == '>') {
                x++;
            }

            char track = grid[y][x];
            if (track == '+') {
                if (intersectionCount % 3 == 0) { // Turn left
                    if (direction == '^') direction = '<';
                    else if (direction == 'v') direction = '>';
                    else if (direction == '<') direction = 'v';
                    else if (direction == '>') direction = '^';
                } else if (intersectionCount % 3 == 2) { // Turn right
                    if (direction == '^') direction = '>';
                    else if (direction == 'v') direction = '<';
                    else if (direction == '<') direction = '^';
                    else if (direction == '>') direction = 'v';
                }
                intersectionCount++;
            } else if (track == '/') {
                if (direction == '^') direction = '>';
                else if (direction == 'v') direction = '<';
                else if (direction == '<') direction = 'v';
                else if (direction == '>') direction = '^';
            } else if (track == '\\') {
                if (direction == '^') direction = '<';
                else if (direction == 'v') direction = '>';
                else if (direction == '<') direction = '^';
                else if (direction == '>') direction = 'v';
            }
        }
    }
}
