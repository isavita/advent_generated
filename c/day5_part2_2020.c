
#include <stdio.h>

int calculateSeatID(char* boardingPass) {
    int row = 0;
    int column = 0;
    for (int i = 0; i < 7; i++) {
        if (boardingPass[i] == 'B') {
            row = (row << 1) | 1;
        } else {
            row = row << 1;
        }
    }
    for (int i = 7; i < 10; i++) {
        if (boardingPass[i] == 'R') {
            column = (column << 1) | 1;
        } else {
            column = column << 1;
        }
    }
    return row * 8 + column;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    int maxSeatID = 0;
    int seatIDs[1024] = {0};

    if (file != NULL) {
        char boardingPass[11];
        while (fscanf(file, "%s", boardingPass) != EOF) {
            int seatID = calculateSeatID(boardingPass);
            seatIDs[seatID] = 1;
            if (seatID > maxSeatID) {
                maxSeatID = seatID;
            }
        }
        fclose(file);
    }

    for (int i = 8; i < 1024 - 8; i++) {
        if (seatIDs[i] == 0 && seatIDs[i - 1] == 1 && seatIDs[i + 1] == 1) {
            printf("%d\n", i);
            break;
        }
    }

    return 0;
}
