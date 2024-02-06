
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOTAL_ROWS 40

int countSafeTiles(char *firstRow, int totalRows);
int isTrap(int left, int center, int right, char *row);
char safeIfOutOfBounds(int index, int len, char *row);
int countChar(char *str, char ch);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }
    char firstRow[1024];
    if (fgets(firstRow, sizeof(firstRow), file) == NULL) {
        perror("Error reading first row");
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);
    // Remove newline character if present
    firstRow[strcspn(firstRow, "\n")] = 0;
    int safeTilesCount = countSafeTiles(firstRow, TOTAL_ROWS);
    printf("%d\n", safeTilesCount);
    return EXIT_SUCCESS;
}

int countSafeTiles(char *firstRow, int totalRows) {
    char currentRow[1024];
    char nextRow[1024];
    strcpy(currentRow, firstRow);
    int safeCount = countChar(currentRow, '.');

    for (int i = 1; i < totalRows; i++) {
        int len = strlen(currentRow);
        for (int j = 0; j < len; j++) {
            if (isTrap(j-1, j, j+1, currentRow)) {
                nextRow[j] = '^';
            } else {
                nextRow[j] = '.';
                safeCount++;
            }
        }
        nextRow[len] = '\0'; // Null-terminate the next row
        strcpy(currentRow, nextRow); // Copy nextRow to currentRow for the next iteration
    }
    return safeCount;
}

int isTrap(int left, int center, int right, char *row) {
    int len = strlen(row);
    char l = safeIfOutOfBounds(left, len, row);
    char c = row[center];
    char r = safeIfOutOfBounds(right, len, row);

    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.');
}

char safeIfOutOfBounds(int index, int len, char *row) {
    if (index < 0 || index >= len) {
        return '.';
    }
    return row[index];
}

int countChar(char *str, char ch) {
    int count = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == ch) {
            count++;
        }
    }
    return count;
}
