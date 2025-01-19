
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 256
#define MAX_NUMBERS 200

typedef struct {
    int winnings[MAX_NUMBERS];
    int num_winnings;
    int givens[MAX_NUMBERS];
    int num_givens;
    int totalCount;
} Card;

int getPointsForCard(const Card *card) {
    int points = 0;
    for (int i = 0; i < card->num_givens; ++i) {
        for (int j = 0; j < card->num_winnings; ++j) {
            if (card->givens[i] == card->winnings[j]) {
                points++;
                break;
            }
        }
    }
    return points;
}

void lexLineIntoCard(const char *line, Card *card) {
    char *cardDataStr = strchr(line, ':') + 2;
    char *separator = strstr(cardDataStr, " | ");
    int len_winnings = separator - cardDataStr;

    card->num_winnings = 0;
    char temp[3];
    int num_index = 0;
    for(int i =0; i < len_winnings; ++i) {
        if(isdigit(cardDataStr[i])) {
             temp[num_index++] = cardDataStr[i];
        } else if(num_index > 0) {
            temp[num_index] = '\0';
            card->winnings[card->num_winnings++] = atoi(temp);
            num_index = 0;
        }
    }

    if(num_index > 0) {
        temp[num_index] = '\0';
        card->winnings[card->num_winnings++] = atoi(temp);
    }


    card->num_givens = 0;
    num_index = 0;
    for(int i = len_winnings + 3; cardDataStr[i] != '\0'; ++i) {
        if(isdigit(cardDataStr[i])) {
             temp[num_index++] = cardDataStr[i];
        } else if(num_index > 0) {
            temp[num_index] = '\0';
            card->givens[card->num_givens++] = atoi(temp);
            num_index = 0;
        }
    }
    if(num_index > 0) {
        temp[num_index] = '\0';
        card->givens[card->num_givens++] = atoi(temp);
    }
    
    card->totalCount = 1;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Card cards[MAX_LINE_LENGTH];
    int num_cards = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n' || line[0] == '\0') continue;
        lexLineIntoCard(line, &cards[num_cards]);
        num_cards++;
    }

    fclose(file);

    for (int i = 0; i < num_cards; ++i) {
        int points = getPointsForCard(&cards[i]);
        for (int j = 1; j <= points && i + j < num_cards; ++j) {
                cards[i+j].totalCount += 1 * cards[i].totalCount;
        }
    }


    int totalCards = 0;
    for (int i = 0; i < num_cards; ++i) {
        totalCards += cards[i].totalCount;
    }

    printf("%d\n", totalCards);

    return 0;
}
