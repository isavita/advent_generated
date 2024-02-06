
#include <stdio.h>
#include <string.h>

void dragon_curve(char *a, int length) {
    char b[length];
    strcpy(b, a);
    int i;
    for (i = 0; i < length; i++) {
        b[length - i - 1] = (a[i] == '0') ? '1' : '0';
    }
    a[length] = '0';
    strcpy(a + length + 1, b);
}

void calculate_checksum(char *data, int length) {
    char checksum[length / 2];
    int i;
    for (i = 0; i < length; i += 2) {
        checksum[i / 2] = (data[i] == data[i + 1]) ? '1' : '0';
    }
    int checksum_length = length / 2;
    while (checksum_length % 2 == 0) {
        for (i = 0; i < checksum_length; i += 2) {
            checksum[i / 2] = (checksum[i] == checksum[i + 1]) ? '1' : '0';
        }
        checksum_length /= 2;
    }
    checksum[checksum_length] = '\0';
    printf("%s\n", checksum);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char initial_state[1000];
    fscanf(file, "%s", initial_state);

    int length = 272;
    while (strlen(initial_state) < length) {
        dragon_curve(initial_state, strlen(initial_state));
    }

    initial_state[length] = '\0';
    calculate_checksum(initial_state, length);

    fclose(file);

    return 0;
}
