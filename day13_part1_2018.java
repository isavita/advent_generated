
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();
        List<Cart> carts = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        char[][] track = new char[lines.size()][];
        for (int i = 0; i < lines.size(); i++) {
            track[i] = new char[lines.get(i).length()];
            for (int j = 0; j < lines.get(i).length(); j++) {
                char s = lines.get(i).charAt(j);
                switch (s) {
                    case '>':
                        track[i][j] = '-';
                        carts.add(new Cart(j, i, '>'));
                        break;
                    case '<':
                        track[i][j] = '-';
                        carts.add(new Cart(j, i, '<'));
                        break;
                    case '^':
                        track[i][j] = '|';
                        carts.add(new Cart(j, i, '^'));
                        break;
                    case 'v':
                        track[i][j] = '|';
                        carts.add(new Cart(j, i, 'v'));
                        break;
                    default:
                        track[i][j] = s;
                        break;
                }
            }
        }

        boolean collision = false;
        while (!collision) {
            for (int i = 0; i < carts.size(); i++) {
                switch (carts.get(i).dir) {
                    case '>':
                        carts.set(i, movingRight(track, carts.get(i)));
                        break;
                    case '<':
                        carts.set(i, movingLeft(track, carts.get(i)));
                        break;
                    case '^':
                        carts.set(i, movingUp(track, carts.get(i)));
                        break;
                    case 'v':
                        carts.set(i, movingDown(track, carts.get(i)));
                        break;
                    default:
                        System.out.println("error not valid cart");
                        break;
                }
            }

            for (int i = 0; i < carts.size(); i++) {
                for (int j = i + 1; j < carts.size(); j++) {
                    if (carts.get(i).x == carts.get(j).x && carts.get(i).y == carts.get(j).y) {
                        collision = true;
                        System.out.printf("%d,%d", carts.get(i).x, carts.get(i).y);
                    }
                }
            }
        }
    }

    static Cart movingDown(char[][] track, Cart cart) {
        switch (track[cart.y + 1][cart.x]) {
            case '/':
                cart.dir = '<';
                break;
            case '\\':
                cart.dir = '>';
                break;
            case '+':
                if (cart.turn == 0) {
                    cart.dir = '>';
                    cart.turn = 1;
                } else if (cart.turn == 1) {
                    cart.turn = 2;
                } else if (cart.turn == 2) {
                    cart.dir = '<';
                    cart.turn = 0;
                }
                break;
            case '|':
                break;
            default:
                System.out.println("Error on track cart can't move : " + cart.x + ", " + (cart.y - 1) + ", " + track[cart.y - 1][cart.x]);
        }
        cart.y = cart.y + 1;
        return cart;
    }

    static Cart movingUp(char[][] track, Cart cart) {
        switch (track[cart.y - 1][cart.x]) {
            case '/':
                cart.dir = '>';
                break;
            case '\\':
                cart.dir = '<';
                break;
            case '+':
                if (cart.turn == 0) {
                    cart.dir = '<';
                    cart.turn = 1;
                } else if (cart.turn == 1) {
                    cart.turn = 2;
                } else if (cart.turn == 2) {
                    cart.dir = '>';
                    cart.turn = 0;
                }
                break;
            case '|':
                break;
            default:
                System.out.println("Error on track cart can't move : " + cart.x + ", " + (cart.y - 1) + ", " + track[cart.y - 1][cart.x]);
        }
        cart.y = cart.y - 1;
        return cart;
    }

    static Cart movingLeft(char[][] track, Cart cart) {
        switch (track[cart.y][cart.x - 1]) {
            case '/':
                cart.dir = 'v';
                break;
            case '\\':
                cart.dir = '^';
                break;
            case '+':
                if (cart.turn == 0) {
                    cart.dir = 'v';
                    cart.turn = 1;
                } else if (cart.turn == 1) {
                    cart.turn = 2;
                } else if (cart.turn == 2) {
                    cart.dir = '^';
                    cart.turn = 0;
                }
                break;
            case '-':
                break;
            default:
                System.out.println("Error on track cart can't move : " + (cart.x - 1) + ", " + cart.y + ", " + track[cart.y][cart.x - 1]);
        }
        cart.x = cart.x - 1;
        return cart;
    }

    static Cart movingRight(char[][] track, Cart cart) {
        switch (track[cart.y][cart.x + 1]) {
            case '\\':
                cart.dir = 'v';
                break;
            case '/':
                cart.dir = '^';
                break;
            case '+':
                if (cart.turn == 0) {
                    cart.dir = '^';
                    cart.turn = 1;
                } else if (cart.turn == 1) {
                    cart.turn = 2;
                } else if (cart.turn == 2) {
                    cart.dir = 'v';
                    cart.turn = 0;
                }
                break;
            case '-':
                break;
            default:
                System.out.println("Error on track cart can't move : " + (cart.x + 1) + ", " + cart.y + ", " + track[cart.y][cart.x + 1]);
        }
        cart.x = cart.x + 1;
        return cart;
    }

    static class Cart {
        int x;
        int y;
        char dir;
        int turn;

        Cart(int x, int y, char dir) {
            this.x = x;
            this.y = y;
            this.dir = dir;
            this.turn = 0;
        }
    }
}
