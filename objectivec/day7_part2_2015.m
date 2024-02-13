#import <Foundation/Foundation.h>

int memoDFS(NSDictionary *graph, NSString *entry, NSMutableDictionary *memo) {
    NSNumber *memoVal = [memo objectForKey:entry];
    if (memoVal != nil) {
        return [memoVal intValue];
    }

    NSError *error = NULL;
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"[0-9]" options:0 error:&error];

    NSString *sourceRule = [graph objectForKey:entry];
    NSArray *parts = [sourceRule componentsSeparatedByString:@" "];

    int result = 0;
    if ([regex numberOfMatchesInString:entry options:0 range:NSMakeRange(0, [entry length])] > 0) {
        result = [entry intValue];
    } else {
        if ([parts count] == 1) {
            result = memoDFS(graph, parts[0], memo);
        } else if ([parts[0] isEqualToString:@"NOT"]) {
            int start = memoDFS(graph, parts[1], memo);
            result = (UINT16_MAX) ^ start;
        } else if ([parts[1] isEqualToString:@"AND"]) {
            result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo);
        } else if ([parts[1] isEqualToString:@"OR"]) {
            result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo);
        } else if ([parts[1] isEqualToString:@"LSHIFT"]) {
            result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo);
        } else if ([parts[1] isEqualToString:@"RSHIFT"]) {
            result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo);
        }
    }

    [memo setObject:@(result) forKey:entry];
    return result;
}

int main() {
    NSError *error = NULL;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error != nil) {
        printf("Error reading from file: %s\n", [[error localizedDescription] UTF8String]);
        return 1;
    }

    NSMutableDictionary *wireToRule = [[NSMutableDictionary alloc] init];
    NSArray *instructions = [input componentsSeparatedByString:@"\n"];

    for (NSString *inst in instructions) {
        NSArray *parts = [inst componentsSeparatedByString:@" -> "];
        [wireToRule setObject:parts[0] forKey:parts[1]];
    }

    int aSignal = memoDFS(wireToRule, @"a", [[NSMutableDictionary alloc] init]);

    [wireToRule setObject:@(aSignal).stringValue forKey:@"b"];
    printf("%d\n", memoDFS(wireToRule, @"a", [[NSMutableDictionary alloc] init]));

    return 0;
}