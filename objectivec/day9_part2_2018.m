
#import <Foundation/Foundation.h>

typedef struct Marble {
    int value;
    struct Marble *prev;
    struct Marble *next;
} Marble;

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        FILE *fp = fopen("input.txt", "r");
        if (!fp) { perror("fopen"); return 1; }
        char line[256];
        if (!fgets(line, sizeof(line), fp)) { perror("fgets"); return 1; }
        fclose(fp);

        int players, lastMarble;
        if (sscanf(line, "%d players; last marble is worth %d points", &players, &lastMarble) != 2)
            return 1;
        lastMarble *= 100;

        long long *scores = calloc(players, sizeof(long long));
        if (!scores) { perror("calloc"); return 1; }

        Marble *current = malloc(sizeof(Marble));
        current->value = 0;
        current->next = current;
        current->prev = current;

        for (int marble = 1; marble <= lastMarble; ++marble) {
            if (marble % 23 == 0) {
                int player = marble % players;
                for (int i = 0; i < 7; ++i) current = current->prev;
                scores[player] += (long long)marble + current->value;
                Marble *toRemove = current;
                current->prev->next = current->next;
                current->next->prev = current->prev;
                current = current->next;
                free(toRemove);
            } else {
                current = current->next;
                Marble *newMarble = malloc(sizeof(Marble));
                newMarble->value = marble;
                newMarble->prev = current;
                newMarble->next = current->next;
                current->next->prev = newMarble;
                current->next = newMarble;
                current = newMarble;
            }
        }

        long long maxScore = 0;
        for (int i = 0; i < players; ++i)
            if (scores[i] > maxScore) maxScore = scores[i];
        printf("%lld\n", maxScore);

        free(scores);
        Marble *tmp = current;
        Marble *head = current->next;
        while (head != current) {
            tmp = head;
            head = head->next;
            free(tmp);
        }
        free(current);
        return 0;
    }
}
