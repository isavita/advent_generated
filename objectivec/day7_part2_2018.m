
#import <Foundation/Foundation.h>

#define NUM_WORKERS 5
#define BASE_DURATION 60

typedef struct {
    NSInteger remaining;
    unichar task;
} Worker;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableDictionary<NSString *, NSMutableSet<NSString *> *> *deps = [NSMutableDictionary dictionary];
        NSMutableSet<NSString *> *allSteps = [NSMutableSet set];
        NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:@"Step (.) must be finished before step (.) can begin\\." options:0 error:nil];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSTextCheckingResult *m = [re firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
            NSString *a = [line substringWithRange:[m rangeAtIndex:1]];
            NSString *b = [line substringWithRange:[m rangeAtIndex:2]];
            [allSteps addObject:a];
            [allSteps addObject:b];
            NSMutableSet *set = deps[b];
            if (!set) { set = [NSMutableSet set]; deps[b] = set; }
            [set addObject:a];
            if (!deps[a]) deps[a] = [NSMutableSet set];
        }

        Worker workers[NUM_WORKERS];
        for (int i = 0; i < NUM_WORKERS; i++) workers[i] = (Worker){0, 0};
        NSInteger time = 0;

        while (allSteps.count) {
            NSMutableArray<NSString *> *available = [NSMutableArray array];
            for (NSString *step in allSteps) {
                if (deps[step].count == 0) {
                    BOOL busy = NO;
                    for (int w = 0; w < NUM_WORKERS; w++) if (workers[w].task == [step characterAtIndex:0]) { busy = YES; break; }
                    if (!busy) [available addObject:step];
                }
            }
            [available sortUsingSelector:@selector(compare:)];

            for (int w = 0; w < NUM_WORKERS && available.count; w++) {
                if (workers[w].remaining == 0) {
                    NSString *next = available[0];
                    [available removeObjectAtIndex:0];
                    unichar c = [next characterAtIndex:0];
                    workers[w].task = c;
                    workers[w].remaining = (c - 'A' + 1) + BASE_DURATION;
                }
            }

            NSInteger min = NSIntegerMax;
            for (int w = 0; w < NUM_WORKERS; w++) if (workers[w].remaining > 0 && workers[w].remaining < min) min = workers[w].remaining;
            if (min == NSIntegerMax) min = 1;
            time += min;

            for (int w = 0; w < NUM_WORKERS; w++) {
                if (workers[w].remaining > 0) {
                    workers[w].remaining -= min;
                    if (workers[w].remaining == 0) {
                        NSString *finished = [NSString stringWithFormat:@"%C", workers[w].task];
                        [allSteps removeObject:finished];
                        for (NSString *key in deps) {
                            NSMutableSet *set = deps[key];
                            [set removeObject:finished];
                        }
                        workers[w].task = 0;
                    }
                }
            }
        }

        printf("%ld\n", (long)time);
    }
    return 0;
}
