
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void reverseSection(int *arr, int start, int length) {
    int n = 256;
    for (int i = start, j = start + length - 1; i < j; i++, j--) {
        int temp = arr[i % n];
        arr[i % n] = arr[j % n];
        arr[j % n] = temp;
    }
}

void knotHash(char *input, char *result) {
    int lengths[64 + 5];
    int len = strlen(input);
    for (int i = 0; i < len; i++) {
        lengths[i] = (int)input[i];
    }
    // Additional lengths
    int additional[] = {17, 31, 73, 47, 23};
    memcpy(lengths + len, additional, sizeof(additional));
    len += 5;

    int list[256];
    for (int i = 0; i < 256; i++) {
        list[i] = i;
    }

    int position = 0, skip = 0;
    for (int round = 0; round < 64; round++) {
        for (int i = 0; i < len; i++) {
            reverseSection(list, position, lengths[i]);
            position += lengths[i] + skip;
            skip++;
        }
    }

    // Dense hash calculation
    int denseHash[16];
    for (int i = 0; i < 16; i++) {
        int xor = 0;
        for (int j = 0; j < 16; j++) {
            xor ^= list[i * 16 + j];
        }
        denseHash[i] = xor;
    }

    // Convert to hexadecimal
    for (int i = 0; i < 16; i++) {
        sprintf(result + (i * 2), "%02x", denseHash[i]);
    }
}

int hexCharToBin(char c) {
    int val = c <= '9' ? c - '0' : c - 'a' + 10;
    return val;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("File reading error\n");
        return 1;
    }

    char keyString[64];
    fgets(keyString, 64, file);
    keyString[strcspn(keyString, "\n")] = 0; // Remove newline
    fclose(file);

    int totalUsed = 0;
    for (int i = 0; i < 128; i++) {
        char rowKey[80];
        sprintf(rowKey, "%s-%d", keyString, i);

        char hash[33];
        knotHash(rowKey, hash);

        for (int j = 0; j < 32; j++) {
            int val = hexCharToBin(hash[j]);
            for (int bit = 0; bit < 4; bit++) {
                if (val & (1 << (3 - bit))) {
                    totalUsed++;
                }
            }
        }
    }

    printf("%d\n", totalUsed);
    return 0;
}
