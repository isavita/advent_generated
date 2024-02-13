#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableDictionary *adj = [NSMutableDictionary dictionary];
        
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" <-> "];
            int from = [parts[0] intValue];
            NSArray *toNodes = [parts[1] componentsSeparatedByString:@", "];
            
            for (NSString *toNode in toNodes) {
                int to = [toNode intValue];
                NSMutableArray *fromList = adj[@(from)];
                if (fromList == nil) {
                    fromList = [NSMutableArray array];
                }
                [fromList addObject:@(to)];
                adj[@(from)] = fromList;
                
                NSMutableArray *toList = adj[@(to)];
                if (toList == nil) {
                    toList = [NSMutableArray array];
                }
                [toList addObject:@(from)];
                adj[@(to)] = toList;
            }
        }
        
        NSMutableDictionary *visited = [NSMutableDictionary dictionary];
        int groups = 0;
        
        for (NSNumber *node in adj.allKeys) {
            if (visited[node] == nil) {
                [visited setObject:@(YES) forKey:node];
                NSMutableArray *stack = [NSMutableArray arrayWithObject:node];
                
                while (stack.count > 0) {
                    NSNumber *currentNode = [stack lastObject];
                    [stack removeLastObject];
                    
                    for (NSNumber *neighbor in adj[currentNode]) {
                        if (visited[neighbor] == nil) {
                            [visited setObject:@(YES) forKey:neighbor];
                            [stack addObject:neighbor];
                        }
                    }
                }
                
                groups++;
            }
        }
        
        printf("%d\n", groups);
    }
    
    return 0;
}