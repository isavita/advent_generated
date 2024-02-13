#import <Foundation/Foundation.h>

int walk(NSMutableDictionary *graph, NSString *current, NSMutableDictionary *visited, NSMutableArray *path, BOOL doubleUsed) {
    if ([current isEqualToString:@"end"]) {
        return 1;
    }

    visited[current] = @([visited[current] intValue] + 1);

    int pathsToEnd = 0;

    for (NSString *visitable in graph[current]) {
        if ([visitable isEqualToString:@"start"]) {
            continue;
        }

        if (![visitable isEqualToString:[visitable uppercaseString]] && [visited[visitable] intValue] > 0) {
            if (doubleUsed) {
                continue;
            } else {
                doubleUsed = YES;
            }
        }

        [path addObject:visitable];
        pathsToEnd += walk(graph, visitable, visited, path, doubleUsed);

        visited[visitable] = @([visited[visitable] intValue] - 1);
        [path removeLastObject];

        if (![visitable isEqualToString:[visitable uppercaseString]] && [visited[visitable] intValue] == 1) {
            doubleUsed = NO;
        }
    }

    return pathsToEnd;
}

NSArray *parseInput(NSString *input) {
    NSMutableArray *ans = [NSMutableArray new];
    for (NSString *line in [input componentsSeparatedByString:@"\n"]) {
        [ans addObject:[line componentsSeparatedByString:@"-"]];
    }
    return ans;
}

int solve(NSString *input) {
    NSArray *parsed = parseInput(input);

    NSMutableDictionary *graph = [NSMutableDictionary new];
    for (NSArray *pair in parsed) {
        if (!graph[pair[0]]) {
            graph[pair[0]] = [NSMutableDictionary new];
        }
        if (!graph[pair[1]]) {
            graph[pair[1]] = [NSMutableDictionary new];
        }
        graph[pair[0]][pair[1]] = @YES;
        graph[pair[1]][pair[0]] = @YES;
    }

    return walk(graph, @"start", [@{@"start": @5} mutableCopy], [@[@"start"] mutableCopy], NO);
}

int main() {
    NSError *error = nil;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"%@", error);
        return 1;
    }

    int ans = solve([input stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]]);
    printf("%d\n", ans);

    return 0;
}