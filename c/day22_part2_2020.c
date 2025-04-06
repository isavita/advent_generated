
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define INITIAL_CAPACITY 10
#define MAX_LINE_LEN 100
#define HASH_TABLE_SIZE 16384 // Power of 2 for modulo optimization

typedef struct {
    int *cards;
    int count;
    int capacity;
} Deck;

typedef struct StateNode {
    char *state_key;
    struct StateNode *next;
} StateNode;

typedef struct {
    StateNode **table;
    int size;
} HashSet;

// Deck functions
void init_deck(Deck *deck) {
    deck->cards = malloc(INITIAL_CAPACITY * sizeof(int));
    if (!deck->cards) {
        perror("Failed to allocate deck");
        exit(EXIT_FAILURE);
    }
    deck->count = 0;
    deck->capacity = INITIAL_CAPACITY;
}

void free_deck(Deck *deck) {
    free(deck->cards);
    deck->cards = NULL;
    deck->count = 0;
    deck->capacity = 0;
}

void push_back(Deck *deck, int card) {
    if (deck->count == deck->capacity) {
        deck->capacity *= 2;
        deck->cards = realloc(deck->cards, deck->capacity * sizeof(int));
        if (!deck->cards) {
            perror("Failed to reallocate deck");
            exit(EXIT_FAILURE);
        }
    }
    deck->cards[deck->count++] = card;
}

int pop_front(Deck *deck) {
    if (deck->count == 0) {
        fprintf(stderr, "Error: Popping from empty deck\n");
        exit(EXIT_FAILURE);
    }
    int card = deck->cards[0];
    memmove(deck->cards, deck->cards + 1, (deck->count - 1) * sizeof(int));
    deck->count--;
    return card;
}

void copy_n(const Deck *source, Deck *dest, int n) {
    init_deck(dest);
    if (n > source->count) n = source->count;
    if (n > dest->capacity) {
         dest->capacity = n;
         dest->cards = realloc(dest->cards, dest->capacity * sizeof(int));
         if (!dest->cards) {
            perror("Failed to reallocate for copy");
            exit(EXIT_FAILURE);
         }
    }
    memcpy(dest->cards, source->cards, n * sizeof(int));
    dest->count = n;
}

long long calculate_score(const Deck *deck) {
    long long score = 0;
    for (int i = 0; i < deck->count; i++) {
        score += (long long)deck->cards[i] * (deck->count - i);
    }
    return score;
}

// HashSet functions
unsigned int hash_function(const char *str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash;
}

void init_hash_set(HashSet *set, int size) {
    set->size = size;
    set->table = calloc(size, sizeof(StateNode *));
     if (!set->table) {
        perror("Failed to allocate hash table");
        exit(EXIT_FAILURE);
    }
}

void free_hash_set(HashSet *set) {
    for (int i = 0; i < set->size; i++) {
        StateNode *current = set->table[i];
        while (current != NULL) {
            StateNode *temp = current;
            current = current->next;
            free(temp->state_key);
            free(temp);
        }
    }
    free(set->table);
}

char* create_state_key(const Deck *p1, const Deck *p2) {
    // Estimate buffer size: num_digits * max_cards + separators + null terminator
    // A generous estimate is fine. Let's assume max card value 99, max 50 cards per deck.
    // (2 * 50) + 1 separator + (2 * 50) + 1 null = 202. Let's use 512 for safety.
    size_t buffer_size = 512;
    char *key = malloc(buffer_size);
     if (!key) {
        perror("Failed to allocate state key buffer");
        exit(EXIT_FAILURE);
    }
    char *ptr = key;
    size_t remaining = buffer_size;
    int written;

    for (int i = 0; i < p1->count; i++) {
        written = snprintf(ptr, remaining, "%d,", p1->cards[i]);
        if (written < 0 || (size_t)written >= remaining) goto error;
        ptr += written;
        remaining -= written;
    }
    if (p1->count > 0) { // Remove trailing comma if exists
       if (*(ptr-1) == ',') {
           ptr--;
           remaining++;
       }
    }

    written = snprintf(ptr, remaining, "|");
    if (written < 0 || (size_t)written >= remaining) goto error;
    ptr += written;
    remaining -= written;

    for (int i = 0; i < p2->count; i++) {
        written = snprintf(ptr, remaining, "%d,", p2->cards[i]);
        if (written < 0 || (size_t)written >= remaining) goto error;
        ptr += written;
        remaining -= written;
    }
     if (p2->count > 0) { // Remove trailing comma if exists
       if (*(ptr-1) == ',') {
           ptr--;
           remaining++;
       }
    }
    *ptr = '\0'; // Null terminate

    return key;

error:
    fprintf(stderr, "Error: State key buffer too small.\n");
    free(key);
    exit(EXIT_FAILURE);
}


bool add_to_set(HashSet *set, const char *state_key) {
    unsigned int index = hash_function(state_key) & (set->size - 1); // Use bitwise AND for power of 2 size
    StateNode *current = set->table[index];
    while (current != NULL) {
        if (strcmp(current->state_key, state_key) == 0) {
            return true; // Already exists
        }
        current = current->next;
    }

    // Add new node
    StateNode *newNode = malloc(sizeof(StateNode));
     if (!newNode) {
        perror("Failed to allocate state node");
        exit(EXIT_FAILURE);
    }
    newNode->state_key = strdup(state_key);
     if (!newNode->state_key) {
        perror("Failed to duplicate state key");
        free(newNode);
        exit(EXIT_FAILURE);
    }
    newNode->next = set->table[index];
    set->table[index] = newNode;
    return false; // Added successfully
}


// Returns true if player 1 wins, false if player 2 wins
bool play_recursive_combat(Deck *p1, Deck *p2) {
    HashSet previous_rounds;
    init_hash_set(&previous_rounds, HASH_TABLE_SIZE);

    while (p1->count > 0 && p2->count > 0) {
        char *round_key = create_state_key(p1, p2);
        if (add_to_set(&previous_rounds, round_key)) {
            free(round_key);
            free_hash_set(&previous_rounds);
            return true; // Player 1 wins due to repeated state
        }
        free(round_key); // Key was duplicated by add_to_set

        int card1 = pop_front(p1);
        int card2 = pop_front(p2);

        bool p1_wins_round;
        if (p1->count >= card1 && p2->count >= card2) {
            Deck sub_p1, sub_p2;
            copy_n(p1, &sub_p1, card1);
            copy_n(p2, &sub_p2, card2);
            p1_wins_round = play_recursive_combat(&sub_p1, &sub_p2);
            free_deck(&sub_p1);
            free_deck(&sub_p2);
        } else {
            p1_wins_round = (card1 > card2);
        }

        if (p1_wins_round) {
            push_back(p1, card1);
            push_back(p1, card2);
        } else {
            push_back(p2, card2);
            push_back(p2, card1);
        }
    }

    free_hash_set(&previous_rounds);
    return p1->count > 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input file");
        return EXIT_FAILURE;
    }

    Deck player1_deck, player2_deck;
    init_deck(&player1_deck);
    init_deck(&player2_deck);
    Deck *current_deck = &player1_deck;

    char line[MAX_LINE_LEN];
    bool reading_player1 = true;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        if (strlen(line) == 0) {
            if (reading_player1) {
                 current_deck = &player2_deck;
                 reading_player1 = false;
            }
            continue;
        }
        if (strncmp(line, "Player", 6) == 0) {
            continue;
        }

        int card = atoi(line);
         if (card > 0) { // Basic validation: atoi returns 0 on error/non-numeric
             push_back(current_deck, card);
         } else if (strcmp(line,"0") == 0) { // Handle card 0 explicitly
             push_back(current_deck, 0);
         }

    }
    fclose(file);

    bool p1_won_game = play_recursive_combat(&player1_deck, &player2_deck);

    Deck *winning_deck = p1_won_game ? &player1_deck : &player2_deck;
    printf("%lld\n", calculate_score(winning_deck));

    free_deck(&player1_deck);
    free_deck(&player2_deck);

    return EXIT_SUCCESS;
}

