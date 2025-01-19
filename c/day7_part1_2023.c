
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_HANDS 1000
#define HAND_SIZE 5

typedef struct {
    char cards[HAND_SIZE + 1];
    int bid;
} Hand;

typedef struct {
    Hand hand;
    long long rank;
} RankedHand;

RankedHand matches[7][MAX_HANDS];
int match_counts[7] = {0};


int card_value(char c) {
    if (isdigit(c)) return c - '0';
    switch (c) {
        case 'T': return 10;
        case 'J': return 11;
        case 'Q': return 12;
        case 'K': return 13;
        case 'A': return 14;
        default: return 0;
    }
}

int hand_type(const char *cards) {
    int counts[15] = {0};
    for (int i = 0; i < HAND_SIZE; i++) {
        counts[card_value(cards[i])]++;
    }
    int pairs = 0, three = 0, four = 0, five = 0;
    for (int i = 2; i <= 14; i++) {
        if (counts[i] == 2) pairs++;
        else if (counts[i] == 3) three++;
        else if (counts[i] == 4) four++;
        else if (counts[i] == 5) five++;
    }

    if (five) return 7;
    if (four) return 6;
    if (three && pairs) return 5;
    if (three) return 4;
    if (pairs == 2) return 3;
    if (pairs) return 2;
    return 1;
}


long long calculate_rank(const char* cards){
    long long rank = 0;
    for(int i = 0; i < HAND_SIZE; i++){
      rank = rank * 16;
      char c = cards[i];
      if (isdigit(c)) rank += c - '0';
        else
            switch(c){
                case 'T': rank += 10; break;
                case 'J': rank += 11; break;
                case 'Q': rank += 12; break;
                case 'K': rank += 13; break;
                case 'A': rank += 14; break;
                
            }
        }
    return rank;
}
void find_matches(Hand *hands, int num_hands) {
  for (int i = 0; i < num_hands; i++) {
        int type = hand_type(hands[i].cards);
        matches[type-1][match_counts[type-1]].hand = hands[i];
        matches[type-1][match_counts[type-1]].rank = calculate_rank(hands[i].cards);
        match_counts[type-1]++;
  }
}


int compare_ranked_hands(const void *a, const void *b) {
    RankedHand *handA = (RankedHand *)a;
    RankedHand *handB = (RankedHand *)b;
    if(handA->rank < handB->rank) return 1;
    if(handA->rank > handB->rank) return -1;
    return 0;
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    Hand hands[MAX_HANDS];
    int num_hands = 0;
    char line[100];

    while (fgets(line, sizeof(line), fp) != NULL) {
        if (strlen(line) <= 1) continue;
        sscanf(line, "%s %d", hands[num_hands].cards, &hands[num_hands].bid);
        num_hands++;
    }

    fclose(fp);

    find_matches(hands, num_hands);

   RankedHand all_ranked_hands[MAX_HANDS];
    int total_ranked_hands = 0;
    for(int i = 6; i >= 0; i--){
        qsort(matches[i], match_counts[i], sizeof(RankedHand), compare_ranked_hands);
        for(int j = 0; j < match_counts[i]; j++){
            all_ranked_hands[total_ranked_hands++] = matches[i][j];
        }
    }

    long long total_winnings = 0;
    for (int i = 0; i < total_ranked_hands; i++) {
        total_winnings += (long long)all_ranked_hands[i].hand.bid * (total_ranked_hands - i);
    }

    printf("%lld\n", total_winnings);
    return 0;
}
