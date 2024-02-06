
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIST_SIZE 256

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    char input[1000];
    fgets(input, 1000, file);
    fclose(file);

    int lengths[1000];
    int length_count = 0;
    for (int i = 0; i < strlen(input); i++) {
        lengths[length_count++] = (int)input[i];
    }
    lengths[length_count++] = 17;
    lengths[length_count++] = 31;
    lengths[length_count++] = 73;
    lengths[length_count++] = 47;
    lengths[length_count++] = 23;

    int list[LIST_SIZE];
    for (int i = 0; i < LIST_SIZE; i++) {
        list[i] = i;
    }

    int current_position = 0;
    int skip_size = 0;

    for (int round = 0; round < 64; round++) {
        for (int i = 0; i < length_count; i++) {
            int length = lengths[i];
            for (int j = 0; j < length/2; j++) {
                int start = (current_position + j) % LIST_SIZE;
                int end = (current_position + length - 1 - j) % LIST_SIZE;
                int temp = list[start];
                list[start] = list[end];
                list[end] = temp;
            }
            current_position = (current_position + length + skip_size) % LIST_SIZE;
            skip_size++;
        }
    }

    unsigned char dense_hash[16];
    for (int i = 0; i < LIST_SIZE; i += 16) {
        int xor = 0;
        for (int j = 0; j < 16; j++) {
            xor ^= list[i+j];
        }
        dense_hash[i/16] = (unsigned char)xor;
    }

    char hex_hash[33];
    for (int i = 0; i < 16; i++) {
        sprintf(hex_hash + i*2, "%02x", dense_hash[i]);
    }
    hex_hash[32] = '\0';

    printf("%s\n", hex_hash);

    return 0;
}
