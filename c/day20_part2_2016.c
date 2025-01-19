
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_RANGES 1000

typedef struct {
    unsigned int start;
    unsigned int end;
} Range;

int compareRanges(const void *a, const void *b) {
    const Range *rangeA = (const Range *)a;
    const Range *rangeB = (const Range *)b;
    if (rangeA->start != rangeB->start) {
        return (rangeA->start > rangeB->start) ? 1 : -1;
    }
    return (rangeA->end > rangeB->end) ? 1 : -1;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    Range ranges[MAX_RANGES];
    int rangeCount = 0;
    char line[32];

    while (fgets(line, sizeof(line), file)) {
       if (sscanf(line, "%u-%u", &ranges[rangeCount].start, &ranges[rangeCount].end) == 2) {
            rangeCount++;
        }
    }
    fclose(file);
    qsort(ranges, rangeCount, sizeof(Range), compareRanges);


    Range mergedRanges[MAX_RANGES];
    int mergedCount = 0;
    mergedRanges[0] = ranges[0];
    mergedCount++;

    for(int i = 1; i < rangeCount; i++){
        if (mergedRanges[mergedCount-1].end >= ranges[i].start -1){
            mergedRanges[mergedCount-1].end = (mergedRanges[mergedCount-1].end > ranges[i].end) ? mergedRanges[mergedCount-1].end : ranges[i].end;
        }else{
            mergedRanges[mergedCount] = ranges[i];
            mergedCount++;
        }
    }

    if (mergedRanges[mergedCount-1].end != UINT_MAX){
        mergedRanges[mergedCount].start = UINT_MAX;
        mergedRanges[mergedCount].end = 0;
        mergedCount++;
    }



    unsigned int totalAllowed = 0;
    for (int i = 1; i < mergedCount; i++) {
        totalAllowed += (mergedRanges[i].start - mergedRanges[i-1].end - 1);
    }


    printf("%u\n", totalAllowed);
    return 0;
}
