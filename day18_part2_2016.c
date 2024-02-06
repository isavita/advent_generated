
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOTAL_ROWS 400000

int countSafeTiles(char *firstRow, int totalRows);
int isTrap(int left, int center, int right, char *row);
char safeIfOutOfBounds(int index, char *row);
int countChar(char *str, char charToCount);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }
    
    char firstRow[100001]; // Assuming first row won't exceed 100000 characters
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
    char *currentRow = strdup(firstRow);
    int safeCount = countChar(currentRow, '.');
    
    for (int i = 1; i < totalRows; i++) {
        char *nextRow = malloc(strlen(currentRow) + 1);
        for (int j = 0; j < strlen(currentRow); j++) {
            if (isTrap(j-1, j, j+1, currentRow)) {
                nextRow[j] = '^';
            } else {
                nextRow[j] = '.';
                safeCount++;
            }
        }
        nextRow[strlen(currentRow)] = '\0'; // Null-terminate the string
        free(currentRow);
        currentRow = nextRow;
    }
    free(currentRow);
    return safeCount;
}

int isTrap(int left, int center, int right, char *row) {
    char l = safeIfOutOfBounds(left, row);
    char c = row[center];
    char r = safeIfOutOfBounds(right, row);
    
    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.');
}

char safeIfOutOfBounds(int index, char *row) {
    if (index < 0 || index >= strlen(row)) {
        return '.';
    }
    return row[index];
}

int countChar(char *str, char charToCount) {
    int count = 0;
    while (*str) {
        if (*str == charToCount) {
            count++;
        }
        str++;
    }
    return count;
}
