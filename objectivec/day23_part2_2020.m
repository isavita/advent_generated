
#import <Foundation/Foundation.h>

#define TOTAL_CUPS 1000000
#define TOTAL_MOVES 10000000

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        const char *input = [content UTF8String];
        int *cups = calloc(TOTAL_CUPS + 1, sizeof(int));
        int lastCup = 0;
        for (int i = 0; input[i] && input[i] != '\n'; ++i) {
            int cup = input[i] - '0';
            if (i) cups[lastCup] = cup;
            lastCup = cup;
        }
        int len = (int)strlen(input);
        for (int i = len + 1; i <= TOTAL_CUPS; ++i) {
            cups[lastCup] = i;
            lastCup = i;
        }
        cups[lastCup] = input[0] - '0';
        int currentCup = input[0] - '0';
        for (int move = 0; move < TOTAL_MOVES; ++move) {
            int p1 = cups[currentCup];
            int p2 = cups[p1];
            int p3 = cups[p2];
            cups[currentCup] = cups[p3];
            int dest = currentCup - 1 ? currentCup - 1 : TOTAL_CUPS;
            while (dest == p1 || dest == p2 || dest == p3) {
                dest = dest - 1 ? dest - 1 : TOTAL_CUPS;
            }
            cups[p3] = cups[dest];
            cups[dest] = p1;
            currentCup = cups[currentCup];
        }
        long long a = cups[1];
        long long b = cups[a];
        printf("%lld\n", a * b);
        free(cups);
    }
    return 0;
}
