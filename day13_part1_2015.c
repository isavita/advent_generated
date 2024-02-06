
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_GUESTS 20
#define MAX_NAME_LEN 20
#define MAX_LINE_LEN 100

int happiness[MAX_GUESTS][MAX_GUESTS];
char guests[MAX_GUESTS][MAX_NAME_LEN];
int numGuests = 0;

int findGuestIndex(char* name) {
    for (int i = 0; i < numGuests; i++) {
        if (strcmp(guests[i], name) == 0) {
            return i;
        }
    }
    strcpy(guests[numGuests], name);
    return numGuests++;
}

void permute(int* arr, int start, int end, int* maxHappiness) {
    if (start == end) {
        int happinessSum = 0;
        for (int i = 0; i <= end; i++) {
            int left = (i + end) % (end + 1);
            int right = (i + 1) % (end + 1);
            happinessSum += happiness[arr[i]][arr[left]] + happiness[arr[i]][arr[right]];
        }
        if (happinessSum > *maxHappiness) {
            *maxHappiness = happinessSum;
        }
        return;
    }
    for (int i = start; i <= end; i++) {
        int temp = arr[start];
        arr[start] = arr[i];
        arr[i] = temp;

        permute(arr, start + 1, end, maxHappiness);

        temp = arr[start];
        arr[start] = arr[i];
        arr[i] = temp;
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        char from[MAX_NAME_LEN], to[MAX_NAME_LEN], action[5];
        int value;
        sscanf(line, "%s would %s %d happiness units by sitting next to %[^.].", from, action, &value, to);
        if (strcmp(action, "lose") == 0) {
            value = -value;
        }
        int fromIndex = findGuestIndex(from);
        int toIndex = findGuestIndex(to);
        happiness[fromIndex][toIndex] = value;
    }
    fclose(file);

    int indices[MAX_GUESTS];
    for (int i = 0; i < numGuests; i++) {
        indices[i] = i;
    }

    int maxHappiness = 0;
    permute(indices, 0, numGuests - 1, &maxHappiness);
    printf("%d\n", maxHappiness);

    return 0;
}
