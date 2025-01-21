
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char cards[6];
    int bid;
} Hand;

int card_value(char c) {
    switch (c) {
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'A': return 10;
        case 'C': return 11;
        case 'D': return 12;
        case 'E': return 13;
        default: return 0;
    }
}

int compare_hands(const void *a, const void *b) {
    return ((int *)b)[0] - ((int *)a)[0];
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Hand hands[1000];
    int hand_count = 0;
    char line[100];

    while (fgets(line, sizeof(line), file)) {
        if (strlen(line) < 6) continue;

        sscanf(line, "%s %d", hands[hand_count].cards, &hands[hand_count].bid);
        hand_count++;
    }

    fclose(file);

    Hand matches[7][1000];
    int match_counts[7] = {0};

    for (int i = 0; i < hand_count; i++) {
        int count[14] = {0};
        for (int j = 0; j < 5; j++) {
            char c = hands[i].cards[j];
            if (c == 'J') c = '1';
            else if (c == 'T') c = 'A';
            else if (c == 'Q') c = 'C';
            else if (c == 'K') c = 'D';
            else if (c == 'A') c = 'E';
            count[card_value(c)]++;
        }
        
        int j_count = count[1];
        count[1] = 0;

        int max_count = 0;
        int max_idx = 0;

        for (int k=2; k<14; k++) {
            if (count[k] > max_count){
                max_count = count[k];
                max_idx = k;
            }
        }

        count[max_idx] += j_count;

        int value = 1;
        for (int j = 0; j < 14; j++) {
            if (count[j] > 0) {
                value *= count[j];
            }
        }

        int match_index;
        if (value == 1) match_index = 6;
        else if (value == 2) match_index = 5;
        else if (value == 3) match_index = 3;
        else if (value == 4) {
            int pair_count = 0;
            for (int j=0; j<14; j++){
                if (count[j] == 2) pair_count++;
            }
            if (pair_count == 2) match_index = 4;
            else match_index = 1;
        }
        else if (value == 5) match_index = 0;
        else if (value == 6) match_index = 2;
        else match_index = 0;

        matches[match_index][match_counts[match_index]] = hands[i];
        match_counts[match_index]++;
    }

    int converted_matches[1000][2];
    int converted_match_count = 0;

    for (int i = 0; i < 7; i++) {
        int temp[1000][2];
        int temp_count = 0;
        for (int j = 0; j < match_counts[i]; j++) {
            char y[6];
            strcpy(y, matches[i][j].cards);
             for (int k = 0; k < 5; k++) {
                if (y[k] == 'J') y[k] = '1';
                else if (y[k] == 'T') y[k] = 'A';
                else if (y[k] == 'Q') y[k] = 'C';
                else if (y[k] == 'K') y[k] = 'D';
                else if (y[k] == 'A') y[k] = 'E';
            }
            int val = (int)strtol(y, NULL, 16);
            temp[temp_count][0] = val;
            temp[temp_count][1] = matches[i][j].bid;
            temp_count++;
        }
        qsort(temp, temp_count, sizeof(temp[0]), compare_hands);
        for (int j = 0; j < temp_count; j++) {
            converted_matches[converted_match_count][0] = temp[j][0];
            converted_matches[converted_match_count][1] = temp[j][1];
            converted_match_count++;
        }
    }

    long long total = 0;
    for (int i = 0; i < converted_match_count; i++) {
        total += (long long)converted_matches[i][1] * (converted_match_count - i);
    }

    printf("%lld\n", total);

    return 0;
}
