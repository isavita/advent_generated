
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_GUESTS 20
#define MAX_NAME_LEN 20
#define INPUT_FILE "input.txt"

typedef struct {
    char name[MAX_NAME_LEN];
    int happiness[MAX_GUESTS];
} Guest;

Guest guests[MAX_GUESTS];
int numGuests = 0;
int maxHappiness = 0;

int findGuestIndex(const char* name) {
    for (int i = 0; i < numGuests; i++) {
        if (strcmp(guests[i].name, name) == 0) {
            return i;
        }
    }
    // Add new guest
    strcpy(guests[numGuests].name, name);
    memset(guests[numGuests].happiness, 0, sizeof(int) * MAX_GUESTS);
    return numGuests++;
}

void readFile(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(1);
    }

    char line[256], from[MAX_NAME_LEN], to[MAX_NAME_LEN], action[5];
    int change;
    while (fgets(line, sizeof(line), file)) {
        sscanf(line, "%s would %s %d happiness units by sitting next to %[^.].", from, action, &change, to);
        if (strcmp(action, "lose") == 0) {
            change = -change;
        }
        int fromIndex = findGuestIndex(from);
        int toIndex = findGuestIndex(to);
        guests[fromIndex].happiness[toIndex] = change;
    }

    fclose(file);
}

void calculateHappiness(int* arrangement, int n, int* happiness) {
    *happiness = 0;
    for (int i = 0; i < n; i++) {
        int left = (i + n - 1) % n;
        int right = (i + 1) % n;
        *happiness += guests[arrangement[i]].happiness[arrangement[left]];
        *happiness += guests[arrangement[i]].happiness[arrangement[right]];
    }
}

void permute(int* arrangement, int start, int n) {
    if (start == n) {
        int happiness;
        calculateHappiness(arrangement, n, &happiness);
        if (happiness > maxHappiness) {
            maxHappiness = happiness;
        }
        return;
    }
    for (int i = start; i < n; i++) {
        int temp = arrangement[start];
        arrangement[start] = arrangement[i];
        arrangement[i] = temp;

        permute(arrangement, start + 1, n);

        temp = arrangement[start];
        arrangement[start] = arrangement[i];
        arrangement[i] = temp;
    }
}

int main() {
    readFile(INPUT_FILE);

    // Add "You" as a neutral guest
    findGuestIndex("You");

    int arrangement[MAX_GUESTS];
    for (int i = 0; i < numGuests; i++) {
        arrangement[i] = i;
    }

    permute(arrangement, 0, numGuests);

    printf("%d\n", maxHappiness);
    return 0;
}
