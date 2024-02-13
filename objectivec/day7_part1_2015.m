#import <Foundation/Foundation.h>

int memoDFS(NSDictionary *graph, NSString *entry, NSMutableDictionary *memo) {
    if ([memo objectForKey:entry]) {
        return [[memo objectForKey:entry] intValue];
    }

    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"[0-9]" options:0 error:nil];
    if ([regex numberOfMatchesInString:entry options:0 range:NSMakeRange(0, [entry length])]) {
        return [entry intValue];
    }

    NSString *sourceRule = [graph objectForKey:entry];
    NSArray *parts = [sourceRule componentsSeparatedByString:@" "];

    int result;
    if ([parts count] == 1) {
        result = memoDFS(graph, [parts objectAtIndex:0], memo);
    } else if ([[parts objectAtIndex:0] isEqualToString:@"NOT"]) {
        int start = memoDFS(graph, [parts objectAtIndex:1], memo);
        result = UINT16_MAX ^ start;
    } else if ([[parts objectAtIndex:1] isEqualToString:@"AND"]) {
        result = memoDFS(graph, [parts objectAtIndex:0], memo) & memoDFS(graph, [parts objectAtIndex:2], memo);
    } else if ([[parts objectAtIndex:1] isEqualToString:@"OR"]) {
        result = memoDFS(graph, [parts objectAtIndex:0], memo) | memoDFS(graph, [parts objectAtIndex:2], memo);
    } else if ([[parts objectAtIndex:1] isEqualToString:@"LSHIFT"]) {
        result = memoDFS(graph, [parts objectAtIndex:0], memo) << memoDFS(graph, [parts objectAtIndex:2], memo);
    } else if ([[parts objectAtIndex:1] isEqualToString:@"RSHIFT"]) {
        result = memoDFS(graph, [parts objectAtIndex:0], memo) >> memoDFS(graph, [parts objectAtIndex:2], memo);
    }

    [memo setObject:[NSNumber numberWithInt:result] forKey:entry];
    return result;
}

int main() {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        printf("Error reading from file: %s\n", [[error localizedDescription] UTF8String]);
        return 1;
    }

    NSMutableDictionary *wireToRule = [NSMutableDictionary dictionary];
    NSArray *inst = [input componentsSeparatedByString:@"\n"];
    for (NSString *line in inst) {
        NSArray *parts = [line componentsSeparatedByString:@" -> "];
        [wireToRule setObject:[parts objectAtIndex:0] forKey:[parts objectAtIndex:1]];
    }

    int aSignal = memoDFS(wireToRule, @"a", [NSMutableDictionary dictionary]);
    printf("%d\n", aSignal);

    return 0;
}