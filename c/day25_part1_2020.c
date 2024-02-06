#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    long card_pub_key, door_pub_key;
    fscanf(fp, "%ld\n%ld", &card_pub_key, &door_pub_key);

    fclose(fp);

    long subject_number = 7;
    long value = 1;
    long loop_size = 0;

    while (value != card_pub_key) {
        value *= subject_number;
        value %= 20201227;
        loop_size++;
    }

    value = 1;
    long encryption_key = 1;

    for (long i = 0; i < loop_size; i++) {
        value *= door_pub_key;
        value %= 20201227;
        encryption_key = value;
    }

    printf("%ld\n", encryption_key);

    return 0;
}