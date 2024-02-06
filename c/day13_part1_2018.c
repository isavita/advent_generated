
#include <stdio.h>

typedef struct {
    int x;
    int y;
    char dir;
    int turn;
} Cart;

Cart MovingRight(char track[1000][1000], Cart cart) {
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
            printf("Error on track cart can't move : %d,%d %c\n", cart.x + 1, cart.y, track[cart.y][cart.x + 1]);
    }
    cart.x = cart.x + 1;
    return cart;
}

Cart MovingLeft(char track[1000][1000], Cart cart) {
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
            printf("Error on track cart can't move : %d,%d %c\n", cart.x - 1, cart.y, track[cart.y][cart.x - 1]);
    }
    cart.x = cart.x - 1;
    return cart;
}

Cart MovingUp(char track[1000][1000], Cart cart) {
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
            printf("Error on track cart can't move : %d,%d %c\n", cart.x, cart.y - 1, track[cart.y - 1][cart.x]);
    }
    cart.y = cart.y - 1;
    return cart;
}

Cart MovingDown(char track[1000][1000], Cart cart) {
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
            printf("Error on track cart can't move : %d,%d %c\n", cart.x, cart.y + 1, track[cart.y + 1][cart.x]);
    }
    cart.y = cart.y + 1;
    return cart;
}

int main() {
    char track[1000][1000];
    Cart carts[1000];
    int num_carts = 0;

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int i = 0;
    int j = 0;
    char c;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            track[i][j] = '\0';
            i++;
            j = 0;
        } else {
            track[i][j] = c;
            j++;
        }

        if (c == '>' || c == '<' || c == '^' || c == 'v') {
            carts[num_carts].x = j - 1;
            carts[num_carts].y = i;
            carts[num_carts].dir = c;
            carts[num_carts].turn = 0;
            num_carts++;
        }
    }
    fclose(file);

    int collision = 0;
    while (!collision) {
        for (int i = 0; i < num_carts; i++) {
            switch (carts[i].dir) {
                case '>':
                    carts[i] = MovingRight(track, carts[i]);
                    break;
                case '<':
                    carts[i] = MovingLeft(track, carts[i]);
                    break;
                case '^':
                    carts[i] = MovingUp(track, carts[i]);
                    break;
                case 'v':
                    carts[i] = MovingDown(track, carts[i]);
                    break;
                default:
                    printf("Error not valid cart\n");
                    break;
            }
        }

        for (int i = 0; i < num_carts; i++) {
            for (int j = i + 1; j < num_carts; j++) {
                if (carts[i].x == carts[j].x && carts[i].y == carts[j].y) {
                    collision = 1;
                    printf("%d,%d\n", carts[i].x, carts[i].y);
                }
            }
        }
    }

    return 0;
}
