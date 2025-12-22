
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <string.h>

#define MAX_NODES 2000
#define MAX_NAME_LEN 51

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableDictionary<NSString *, NSNumber *> *dict = [NSMutableDictionary dictionary];
        static char nodeNames[MAX_NODES][MAX_NAME_LEN];
        static bool adj[MAX_NODES][MAX_NODES];
        int nodeCount = 0;

        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        for (NSString *rawLine in [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            if (rawLine.length == 0) continue;
            NSRange dash = [rawLine rangeOfString:@"-"];
            if (dash.location == NSNotFound) continue;
            NSString *n1 = [rawLine substringToIndex:dash.location];
            NSString *n2 = [rawLine substringFromIndex:dash.location + 1];
            if (n1.length == 0 || n2.length == 0) continue;

            NSNumber *id1 = dict[n1];
            if (!id1) {
                dict[n1] = @(nodeCount);
                strncpy(nodeNames[nodeCount], [n1 UTF8String], MAX_NAME_LEN - 1);
                nodeNames[nodeCount][MAX_NAME_LEN - 1] = '\0';
                id1 = @(nodeCount++);
            }
            NSNumber *id2 = dict[n2];
            if (!id2) {
                dict[n2] = @(nodeCount);
                strncpy(nodeNames[nodeCount], [n2 UTF8String], MAX_NAME_LEN - 1);
                nodeNames[nodeCount][MAX_NAME_LEN - 1] = '\0';
                id2 = @(nodeCount++);
            }

            int i = [id1 intValue];
            int j = [id2 intValue];
            adj[i][j] = adj[j][i] = true;
        }

        int count = 0;
        for (int i = 0; i < nodeCount; ++i) {
            for (int j = i + 1; j < nodeCount; ++j) if (adj[i][j]) {
                for (int k = j + 1; k < nodeCount; ++k) if (adj[j][k] && adj[k][i]) {
                    if (nodeNames[i][0] == 't' || nodeNames[j][0] == 't' || nodeNames[k][0] == 't')
                        ++count;
                }
            }
        }
        printf("%d\n", count);
    }
    return 0;
}
