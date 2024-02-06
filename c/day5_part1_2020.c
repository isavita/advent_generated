
#include <stdio.h>

int binary_search(char *pass, int low, int high, char lower_char, char upper_char) {
    for (int i = 0; pass[i] != '\0'; i++) {
        int mid = low + (high - low) / 2;
        if (pass[i] == lower_char) {
            high = mid;
        } else if (pass[i] == upper_char) {
            low = mid + 1;
        }
    }
    return low;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char pass[11];
    int max_seat_id = 0;
    while (fscanf(file, "%s", pass) != EOF) {
        int row = binary_search(pass, 0, 127, 'F', 'B');
        int col = binary_search(pass + 7, 0, 7, 'L', 'R');
        int seat_id = row * 8 + col;
        if (seat_id > max_seat_id) {
            max_seat_id = seat_id;
        }
    }

    fclose(file);
    
    printf("%d", max_seat_id);

    return 0;
}
