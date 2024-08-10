#import <Foundation/Foundation.h>

// Function to read input from the file and return a list of edges
NSArray<NSString *> *readInputFromFile(NSString *filePath) {
    NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray<NSString *> *lines = [fileContent componentsSeparatedByString:@"\n"];
    return lines;
}

// Function to find all paths
void findPaths(NSMutableDictionary<NSString *, NSMutableSet<NSString *> *> *graph, NSString *currentNode, NSMutableSet<NSString *> *visited, NSMutableArray<NSArray<NSString *> *> *paths, NSArray<NSString *> *currentPath) {
    [visited addObject:currentNode];
    NSMutableArray<NSString *> *newPath = [NSMutableArray arrayWithArray:currentPath];
    [newPath addObject:currentNode];

    if ([currentNode isEqualToString:@"end"]) {
        [paths addObject:newPath];
    } else {
        for (NSString *neighbor in graph[currentNode]) {
            if (![visited containsObject:neighbor] || [neighbor isEqualToString:neighbor.uppercaseString]) {
                findPaths(graph, neighbor, visited, paths, newPath);
            }
        }
    }

    [visited removeObject:currentNode];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSArray<NSString *> *lines = readInputFromFile(filePath);

        // Build the graph
        NSMutableDictionary<NSString *, NSMutableSet<NSString *> *> *graph = [NSMutableDictionary dictionary];
        for (NSString *line in lines) {
            NSArray<NSString *> *nodes = [line componentsSeparatedByString:@"-"];
            if (nodes.count == 2) {
                NSString *node1 = nodes[0];
                NSString *node2 = nodes[1];

                if (!graph[node1]) {
                    graph[node1] = [NSMutableSet set];
                }
                if (!graph[node2]) {
                    graph[node2] = [NSMutableSet set];
                }
                [graph[node1] addObject:node2];
                [graph[node2] addObject:node1];
            } else {
                NSLog(@"Invalid line format: %@", line);
            }
        }

        // Find all paths
        NSMutableArray<NSArray<NSString *> *> *paths = [NSMutableArray array];
        findPaths(graph, @"start", [NSMutableSet set], paths, @[]);

        // Print the number of paths
        NSLog(@"Number of paths: %lu", (unsigned long)paths.count);
    }
    return 0;
}