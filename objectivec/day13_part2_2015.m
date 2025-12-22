
#import <Foundation/Foundation.h>

#define MAX_GUESTS 20
#define MAX_NAME_LEN 20

typedef struct {
    char name[MAX_NAME_LEN];
    int happiness[MAX_GUESTS];
} Guest;

static Guest guests[MAX_GUESTS];
static int numGuests = 0;
static int maxHappiness = 0;

static int findGuestIndex(const char *name) {
    for (int i = 0; i < numGuests; i++) {
        if (strcmp(guests[i].name, name) == 0) return i;
    }
    strncpy(guests[numGuests].name, name, MAX_NAME_LEN - 1);
    memset(guests[numGuests].happiness, 0, sizeof(int) * MAX_GUESTS);
    return numGuests++;
}

static void calculateHappiness(int *arr, int n, int *out) {
    int total = 0;
    for (int i = 0; i < n; i++) {
        int left = (i + n - 1) % n;
        int right = (i + 1) % n;
        total += guests[arr[i]].happiness[arr[left]];
        total += guests[arr[i]].happiness[arr[right]];
    }
    *out = total;
}

static void permute(int *arr, int start, int n) {
    if (start == n) {
        int h;
        calculateHappiness(arr, n, &h);
        if (h > maxHappiness) maxHappiness = h;
        return;
    }
    for (int i = start; i < n; i++) {
        int tmp = arr[start];
        arr[start] = arr[i];
        arr[i] = tmp;
        permute(arr, start + 1, n);
        tmp = arr[start];
        arr[start] = arr[i];
        arr[i] = tmp;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        [content enumerateLinesUsingBlock:^(NSString *line, BOOL *stop) {
            char from[MAX_NAME_LEN], to[MAX_NAME_LEN], action[5];
            int change;
            sscanf([line UTF8String], "%s would %s %d happiness units by sitting next to %[^.].", from, action, &change, to);
            if (strcmp(action, "lose") == 0) change = -change;
            int fi = findGuestIndex(from);
            int ti = findGuestIndex(to);
            guests[fi].happiness[ti] = change;
        }];

        findGuestIndex("You");

        int arrangement[MAX_GUESTS];
        for (int i = 0; i < numGuests; i++) arrangement[i] = i;

        permute(arrangement, 0, numGuests);
        printf("%d\n", maxHappiness);
    }
    return 0;
}
