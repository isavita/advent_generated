#import <Foundation/Foundation.h>

NSMutableDictionary<NSString *, NSMutableArray<NSString *> *> *parseInput(NSString *filename, NSMutableSet<NSString *> *allSteps) {
    NSMutableDictionary<NSString *, NSMutableArray<NSString *> *> *deps = [NSMutableDictionary dictionary];
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
    
    for (NSString *line in lines) {
        if (line.length == 0) continue;
        NSString *a = [line substringWithRange:NSMakeRange(5, 1)];
        NSString *b = [line substringWithRange:NSMakeRange(36, 1)];
        if (!deps[b]) deps[b] = [NSMutableArray array];
        [deps[b] addObject:a];
        [allSteps addObject:a];
        [allSteps addObject:b];
    }
    return deps;
}

NSString *topologicalSort(NSMutableDictionary<NSString *, NSMutableArray<NSString *> *> *deps, NSMutableSet<NSString *> *allSteps) {
    NSMutableString *order = [NSMutableString string];
    NSMutableArray<NSString *> *available = [NSMutableArray array];
    
    for (NSString *step in allSteps) {
        if (deps[step].count == 0) [available addObject:step];
    }
    
    [available sortUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
    
    while (available.count > 0) {
        NSString *next = available[0];
        [available removeObjectAtIndex:0];
        [order appendString:next];
        
        for (NSString *step in allSteps) {
            if ([deps[step] containsObject:next]) {
                [deps[step] removeObject:next];
                if (deps[step].count == 0) [available addObject:step];
            }
        }
        [available sortUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
    }
    return order;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableSet<NSString *> *allSteps = [NSMutableSet set];
        NSMutableDictionary<NSString *, NSMutableArray<NSString *> *> *deps = parseInput(@"input.txt", allSteps);
        NSString *order = topologicalSort(deps, allSteps);
        NSLog(@"%@", order);
    }
    return 0;
}