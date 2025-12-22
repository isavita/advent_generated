
#import <Foundation/Foundation.h>

#define MAX_GUESTS 20
#define MAX_LINE_LEN 256

static int happiness[MAX_GUESTS][MAX_GUESTS];
static char guests[MAX_GUESTS][32];
static int numGuests = 0;

static int findGuestIndex(const char *name) {
    for (int i = 0; i < numGuests; i++) {
        if (strcmp(guests[i], name) == 0) return i;
    }
    strcpy(guests[numGuests], name);
    return numGuests++;
}

static void permute(int *arr, int start, int end, int *maxHappiness) {
    if (start == end) {
        int sum = 0;
        int n = end + 1;
        for (int i = 0; i < n; i++) {
            int left = (i + n - 1) % n;
            int right = (i + 1) % n;
            sum += happiness[arr[i]][arr[left]] + happiness[arr[i]][arr[right]];
        }
        if (sum > *maxHappiness) *maxHappiness = sum;
        return;
    }
    for (int i = start; i <= end; i++) {
        int tmp = arr[start];
        arr[start] = arr[i];
        arr[i] = tmp;

        permute(arr, start + 1, end, maxHappiness);

        tmp = arr[start];
        arr[start] = arr[i];
        arr[i] = tmp;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (!file) return 1;
        char line[MAX_LINE_LEN];
        while (fgets(line, sizeof(line), file)) {
            char from[32], to[32], action[8];
            int value;
            sscanf(line, "%s would %s %d happiness units by sitting next to %[^.].", from, action, &value, to);
            if (strcmp(action, "lose") == 0) value = -value;
            int fromIdx = findGuestIndex(from);
            int toIdx = findGuestIndex(to);
            happiness[fromIdx][toIdx] = value;
        }
        fclose(file);

        int indices[MAX_GUESTS];
        for (int i = 0; i < numGuests; i++) indices[i] = i;

        // fix first guest to reduce symmetric permutations
        int maxHappiness = 0;
        permute(indices, 1, numGuests - 1, &maxHappiness);
        printf("%d\n", maxHappiness);
    }
    return 0;
}
