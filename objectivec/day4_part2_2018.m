#import <Foundation/Foundation.h>

#define MAX_GUARDS 100

typedef struct {
    int id;
    int minutes[60];
    int total;
} Guard;

static Guard guards[MAX_GUARDS];
static int guardCount = 0;

static int findOrAdd(int gid) {
    for (int i = 0; i < guardCount; ++i)
        if (guards[i].id == gid) return i;
    guards[guardCount].id = gid;
    guards[guardCount].total = 0;
    memset(guards[guardCount].minutes, 0, sizeof(guards[0].minutes));
    return guardCount++;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        NSArray *lines = [[txt componentsSeparatedByString:@"\n"]
                          sortedArrayUsingSelector:@selector(compare:)];
        int current = -1, asleep = -1;
        for (NSString *l in lines) {
            if (l.length < 18) continue;
            int min = (int)[l substringWithRange:NSMakeRange(15, 2)].intValue;
            NSRange h = [l rangeOfString:@"#"];
            if (h.location != NSNotFound) {
                current = findOrAdd((int)[[l substringFromIndex:h.location+1] intValue]);
            } else if ([l containsString:@"falls"]) {
                asleep = min;
            } else if ([l containsString:@"wakes"]) {
                for (int m = asleep; m < min; ++m) {
                    ++guards[current].minutes[m];
                    ++guards[current].total;
                }
            }
        }
        // Part 1
        int best = 0, bestMin = 0, bestId = 0;
        for (int i = 0; i < guardCount; ++i) {
            if (guards[i].total <= best) continue;
            best = guards[i].total;
            bestId = guards[i].id;
            int mx = 0;
            for (int m = 0; m < 60; ++m)
                if (guards[i].minutes[m] > guards[i].minutes[mx]) mx = m;
            bestMin = mx;
        }
        printf("%d\n", bestId * bestMin);
        // Part 2
        best = 0;
        for (int i = 0; i < guardCount; ++i)
            for (int m = 0; m < 60; ++m)
                if (guards[i].minutes[m] > best) {
                    best = guards[i].minutes[m];
                    bestId = guards[i].id;
                    bestMin = m;
                }
        printf("%d\n", bestId * bestMin);
    }
    return 0;
}