
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Size 10007

int* dealIntoNewStack(int* deck) {
    for (int i = 0; i < Size/2; i++) {
        int temp = deck[i];
        deck[i] = deck[Size-i-1];
        deck[Size-i-1] = temp;
    }
    return deck;
}

int* cutN(int* deck, int n) {
    int* newDeck = (int*)malloc(Size * sizeof(int));

    if (n >= 0) {
        memcpy(newDeck, deck + n, (Size - n) * sizeof(int));
        memcpy(newDeck + (Size - n), deck, n * sizeof(int));
    } else {
        memcpy(newDeck, deck + (Size + n), (-n) * sizeof(int));
        memcpy(newDeck + (-n), deck, (Size + n) * sizeof(int));
    }

    return newDeck;
}

int* dealWithIncrement(int* deck, int n) {
    int* newDeck = (int*)malloc(Size * sizeof(int));

    for (int i = 0; i < Size; i++) {
        newDeck[(i*n)%Size] = deck[i];
    }

    return newDeck;
}

int find2019(int* deck) {
    for (int i = 0; i < Size; i++) {
        if (deck[i] == 2019) {
            return i;
        }
    }
    return -1;
}

int main() {
    int* deck = (int*)malloc(Size * sizeof(int));
    for (int i = 0; i < Size; i++) {
        deck[i] = i;
    }

    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[100];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;

        if (strcmp(line, "deal into new stack") == 0) {
            deck = dealIntoNewStack(deck);
            continue;
        }

        if (strncmp(line, "cut", 3) == 0) {
            int n = atoi(line + 4);
            deck = cutN(deck, n);
            continue;
        }

        if (strncmp(line, "deal with increment", 19) == 0) {
            int n = atoi(line + 20);
            deck = dealWithIncrement(deck, n);
            continue;
        }
    }

    printf("%d\n", find2019(deck));

    free(deck);
    fclose(file);

    return 0;
}
