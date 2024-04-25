#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define screenWidth 50
#define screenHeight 6

typedef struct {
    int width;
    int height;
    char **pixels;
} Screen;

void initScreen(Screen *screen) {
    screen->width = screenWidth;
    screen->height = screenHeight;
    screen->pixels = (char **)malloc(screenHeight * sizeof(char *));
    for (int i = 0; i < screenHeight; i++) {
        screen->pixels[i] = (char *)calloc(screenWidth, sizeof(char));
    }
}

void processInstruction(Screen *screen, char *instruction) {
    if (strncmp(instruction, "rect ", 5) == 0) {
        int a, b;
        sscanf(instruction, "rect %dx%d", &a, &b);
        rect(screen, a, b);
    } else if (strncmp(instruction, "rotate row y=", 13) == 0) {
        int a, b;
        sscanf(instruction, "rotate row y=%d by %d", &a, &b);
        rotateRow(screen, a, b);
    } else if (strncmp(instruction, "rotate column x=", 15) == 0) {
        int a, b;
        sscanf(instruction, "rotate column x=%d by %d", &a, &b);
        rotateColumn(screen, a, b);
    }
}

void rect(Screen *screen, int a, int b) {
    for (int y = 0; y < b; y++) {
        for (int x = 0; x < a; x++) {
            screen->pixels[y][x] = '#';
        }
    }
}

void rotateRow(Screen *screen, int row, int shift) {
    char temp[screenWidth];
    for (int i = 0; i < screenWidth; i++) {
        temp[(i + shift) % screenWidth] = screen->pixels[row][i];
    }
    for (int i = 0; i < screenWidth; i++) {
        screen->pixels[row][i] = temp[i];
    }
}

void rotateColumn(Screen *screen, int col, int shift) {
    char temp[screenHeight];
    for (int i = 0; i < screenHeight; i++) {
        temp[(i + shift) % screenHeight] = screen->pixels[i][col];
    }
    for (int i = 0; i < screenHeight; i++) {
        screen->pixels[i][col] = temp[i];
    }
}

void displayScreen(Screen *screen) {
    for (int i = 0; i < screenHeight; i++) {
        for (int j = 0; j < screenWidth; j++) {
            printf("%c", screen->pixels[i][j] == '#' ? '#' : '.');
        }
        printf("\n");
    }
}

int countLitPixels(Screen *screen) {
    int count = 0;
    for (int i = 0; i < screenHeight; i++) {
        for (int j = 0; j < screenWidth; j++) {
            if (screen->pixels[i][j] == '#') {
                count++;
            }
        }
    }
    return count;
}

int main() {
    Screen screen;
    initScreen(&screen);

    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char instruction[100];
    while (fgets(instruction, 100, file)) {
        processInstruction(&screen, instruction);
    }

    fclose(file);

    displayScreen(&screen);
    printf("Lit pixels: %d\n", countLitPixels(&screen));

    for (int i = 0; i < screenHeight; i++) {
        free(screen.pixels[i]);
    }
    free(screen.pixels);

    return 0;
}