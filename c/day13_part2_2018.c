#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
    char direction;
    int turns;
} Cart;

typedef struct {
    Cart **carts;
    int count;
} CartList;

void moveCart(Cart *cart, char **tracks) {
    switch (cart->direction) {
        case '>': cart->x++; break;
        case '<': cart->x--; break;
        case '^': cart->y--; break;
        case 'v': cart->y++; break;
    }
    switch (tracks[cart->y][cart->x]) {
        case '+': 
            if (cart->turns % 3 == 0) cart->direction = (cart->direction == '>') ? '^' : (cart->direction == '<') ? 'v' : (cart->direction == '^') ? '<' : '>';
            else if (cart->turns % 3 == 2) cart->direction = (cart->direction == '>') ? 'v' : (cart->direction == '<') ? '^' : (cart->direction == '^') ? '>' : '<';
            cart->turns++;
            break;
        case '/':
            cart->direction = (cart->direction == '>') ? '^' : (cart->direction == '<') ? 'v' : (cart->direction == '^') ? '>' : '<';
            break;
        case '\\':
            cart->direction = (cart->direction == '>') ? 'v' : (cart->direction == '<') ? '^' : (cart->direction == '^') ? '<' : '>';
            break;
    }
}

int checkCrash(Cart *cart, CartList *cartList) {
    for (int i = 0; i < cartList->count; i++) {
        Cart *c = cartList->carts[i];
        if (c != cart && c->x == cart->x && c->y == cart->y) {
            return i;
        }
    }
    return -1;
}

void sortCarts(CartList *cartList) {
    for (int i = 0; i < cartList->count - 1; i++) {
        for (int j = i + 1; j < cartList->count; j++) {
            if (cartList->carts[i]->y > cartList->carts[j]->y || (cartList->carts[i]->y == cartList->carts[j]->y && cartList->carts[i]->x > cartList->carts[j]->x)) {
                Cart *temp = cartList->carts[i];
                cartList->carts[i] = cartList->carts[j];
                cartList->carts[j] = temp;
            }
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char **tracks = NULL;
    CartList cartList = {NULL, 0};
    char line[256];
    int y = 0;

    while (fgets(line, sizeof(line), file)) {
        tracks = realloc(tracks, sizeof(char*) * (y + 1));
        tracks[y] = strdup(line);
        for (int x = 0; line[x]; x++) {
            if (strchr("><^v", line[x])) {
                cartList.carts = realloc(cartList.carts, sizeof(Cart*) * (cartList.count + 1));
                cartList.carts[cartList.count] = malloc(sizeof(Cart));
                cartList.carts[cartList.count]->x = x;
                cartList.carts[cartList.count]->y = y;
                cartList.carts[cartList.count]->direction = line[x];
                cartList.carts[cartList.count]->turns = 0;
                cartList.count++;
                line[x] = (line[x] == '>' || line[x] == '<') ? '-' : '|';
            }
        }
        y++;
    }
    fclose(file);

    while (cartList.count > 1) {
        sortCarts(&cartList);
        int *toRemove = calloc(cartList.count, sizeof(int));

        for (int i = 0; i < cartList.count; i++) {
            if (toRemove[i]) continue;
            moveCart(cartList.carts[i], tracks);
            int crashIndex = checkCrash(cartList.carts[i], &cartList);
            if (crashIndex != -1) {
                toRemove[i] = 1;
                toRemove[crashIndex] = 1;
            }
        }

        Cart **newCarts = malloc(sizeof(Cart*) * cartList.count);
        int newCount = 0;
        for (int i = 0; i < cartList.count; i++) {
            if (!toRemove[i]) newCarts[newCount++] = cartList.carts[i];
        }
        free(cartList.carts);
        cartList.carts = newCarts;
        cartList.count = newCount;
        free(toRemove);
    }

    printf("%d,%d\n", cartList.carts[0]->x, cartList.carts[0]->y);
    for (int i = 0; i < cartList.count; i++) free(cartList.carts[i]);
    free(cartList.carts);
    for (int i = 0; i < y; i++) free(tracks[i]);
    free(tracks);
    return 0;
}