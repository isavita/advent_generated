
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void reverseSection(int* arr, int start, int length, int n) {
    for (int i = start, j = start + length - 1; i < j; i++, j--) {
        int temp = arr[i % n];
        arr[i % n] = arr[j % n];
        arr[j % n] = temp;
    }
}

void knotHash(const char* input, char* result) {
    int lengths[256 + 5];
    int len = strlen(input);
    for (int i = 0; i < len; i++) {
        lengths[i] = input[i];
    }
    lengths[len] = 17;
    lengths[len + 1] = 31;
    lengths[len + 2] = 73;
    lengths[len + 3] = 47;
    lengths[len + 4] = 23;
    len += 5;

    int list[256];
    for (int i = 0; i < 256; i++) {
        list[i] = i;
    }

    int position = 0, skip = 0;
    for (int round = 0; round < 64; round++) {
        for (int i = 0; i < len; i++) {
            reverseSection(list, position, lengths[i], 256);
            position += lengths[i] + skip;
            skip++;
        }
    }

    int denseHash[16];
    for (int i = 0; i < 16; i++) {
        int xor = 0;
        for (int j = 0; j < 16; j++) {
            xor ^= list[i * 16 + j];
        }
        denseHash[i] = xor;
    }

    for (int i = 0; i < 16; i++) {
        sprintf(&result[i*2], "%02x", denseHash[i]);
    }
}

void hexToBinary(const char* hexStr, char* binaryStr) {
    for (int i = 0; hexStr[i] != '\0'; i++) {
        int num = 0;
        if (hexStr[i] >= '0' && hexStr[i] <= '9') {
            num = hexStr[i] - '0';
        } else {
            num = hexStr[i] - 'a' + 10;
        }
        for (int j = 3; j >= 0; j--) {
            binaryStr[i * 4 + (3 - j)] = (num & (1 << j)) ? '1' : '0';
        }
    }
    binaryStr[strlen(hexStr) * 4] = '\0';
}

void dfs(int x, int y, int grid[128][128]) {
    if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1) {
        return;
    }
    grid[x][y] = 0;
    dfs(x - 1, y, grid);
    dfs(x + 1, y, grid);
    dfs(x, y - 1, grid);
    dfs(x, y + 1, grid);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }
    char keyString[256];
    fgets(keyString, 256, file);
    keyString[strcspn(keyString, "\n")] = 0; // Remove newline
    fclose(file);

    int grid[128][128] = {{0}};
    int totalUsed = 0, regions = 0;

    for (int i = 0; i < 128; i++) {
        char rowKey[300];
        sprintf(rowKey, "%s-%d", keyString, i);
        char hash[33];
        knotHash(rowKey, hash);
        char binaryRow[129];
        hexToBinary(hash, binaryRow);

        for (int j = 0; binaryRow[j] != '\0'; j++) {
            if (binaryRow[j] == '1') {
                grid[i][j] = 1;
                totalUsed++;
            }
        }
    }

    for (int i = 0; i < 128; i++) {
        for (int j = 0; j < 128; j++) {
            if (grid[i][j] == 1) {
                regions++;
                dfs(i, j, grid);
            }
        }
    }

    printf("%d\n", regions);
    return 0;
}
