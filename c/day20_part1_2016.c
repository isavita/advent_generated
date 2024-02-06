
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    uint32_t start;
    uint32_t end;
} ipRange;

int compare(const void *a, const void *b) {
    ipRange *ipA = (ipRange *)a;
    ipRange *ipB = (ipRange *)b;
    return (ipA->start > ipB->start) - (ipA->start < ipB->start);
}

ipRange *readIPRanges(const char *filename, int *count) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Unable to open file");
        exit(1);
    }

    ipRange *ranges = NULL;
    *count = 0;
    char line[100];
    while (fgets(line, sizeof(line), file)) {
        ranges = realloc(ranges, (*count + 1) * sizeof(ipRange));
        uint32_t start, end;
        sscanf(line, "%u-%u", &start, &end);
        ranges[*count].start = start;
        ranges[*count].end = end;
        (*count)++;
    }
    fclose(file);
    return ranges;
}

uint32_t findUnblockedIP(ipRange *ranges, int count) {
    uint32_t currentIP = 0;
    for (int i = 0; i < count; i++) {
        if (ranges[i].start > currentIP) {
            return currentIP;
        }
        if (ranges[i].end >= currentIP) {
            currentIP = ranges[i].end + 1;
        }
    }
    return currentIP;
}

int main() {
    int count;
    ipRange *ipRanges = readIPRanges("input.txt", &count);
    qsort(ipRanges, count, sizeof(ipRange), compare);

    uint32_t unblockedIP = findUnblockedIP(ipRanges, count);
    printf("%u\n", unblockedIP);

    free(ipRanges);
    return 0;
}
