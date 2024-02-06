
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SCREEN_WIDTH 50
#define SCREEN_HEIGHT 6

void processInstruction(char instruction[], int screen[SCREEN_HEIGHT][SCREEN_WIDTH]);
void rect(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int a, int b);
void rotateRow(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int row, int shift);
void rotateColumn(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int col, int shift);
int countLitPixels(int screen[SCREEN_HEIGHT][SCREEN_WIDTH]);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int screen[SCREEN_HEIGHT][SCREEN_WIDTH] = {0};

    char instruction[100];
    while (fgets(instruction, 100, file) != NULL) {
        processInstruction(instruction, screen);
    }

    printf("%d\n", countLitPixels(screen));

    fclose(file);
    return 0;
}

void processInstruction(char instruction[], int screen[SCREEN_HEIGHT][SCREEN_WIDTH]) {
    char *rectRegex = "rect (\\d+)x(\\d+)";
    char *rotateRowRegex = "rotate row y=(\\d+) by (\\d+)";
    char *rotateColumnRegex = "rotate column x=(\\d+) by (\\d+)";

    if (strstr(instruction, "rect") != NULL) {
        int a, b;
        sscanf(instruction, "rect %dx%d", &a, &b);
        rect(screen, a, b);
    } else if (strstr(instruction, "rotate row") != NULL) {
        int row, shift;
        sscanf(instruction, "rotate row y=%d by %d", &row, &shift);
        rotateRow(screen, row, shift);
    } else if (strstr(instruction, "rotate column") != NULL) {
        int col, shift;
        sscanf(instruction, "rotate column x=%d by %d", &col, &shift);
        rotateColumn(screen, col, shift);
    }
}

void rect(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int a, int b) {
    for (int y = 0; y < b; y++) {
        for (int x = 0; x < a; x++) {
            screen[y][x] = 1;
        }
    }
}

void rotateRow(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int row, int shift) {
    int temp[SCREEN_WIDTH];
    for (int i = 0; i < SCREEN_WIDTH; i++) {
        temp[(i + shift) % SCREEN_WIDTH] = screen[row][i];
    }
    for (int i = 0; i < SCREEN_WIDTH; i++) {
        screen[row][i] = temp[i];
    }
}

void rotateColumn(int screen[SCREEN_HEIGHT][SCREEN_WIDTH], int col, int shift) {
    int temp[SCREEN_HEIGHT];
    for (int i = 0; i < SCREEN_HEIGHT; i++) {
        temp[(i + shift) % SCREEN_HEIGHT] = screen[i][col];
    }
    for (int i = 0; i < SCREEN_HEIGHT; i++) {
        screen[i][col] = temp[i];
    }
}

int countLitPixels(int screen[SCREEN_HEIGHT][SCREEN_WIDTH]) {
    int count = 0;
    for (int i = 0; i < SCREEN_HEIGHT; i++) {
        for (int j = 0; j < SCREEN_WIDTH; j++) {
            if (screen[i][j] == 1) {
                count++;
            }
        }
    }
    return count;
}
