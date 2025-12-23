
#import <Foundation/Foundation.h>

#define MAX_NODES 1000
#define MAX_INSTR_LEN 1000
#define MAX_LINE_LEN 100
#define NODE_NAME_LEN 3
#define HASH_SIZE 17576 // 26 * 26 * 26 for AAA to ZZZ

typedef struct {
    char name[NODE_NAME_LEN + 1];
    char left[NODE_NAME_LEN + 1];
    char right[NODE_NAME_LEN + 1];
} Node;

int nameToInt(const char* name) {
    if (!name || strlen(name) != NODE_NAME_LEN) return -1;
    return (name[0] - 'A') * 26 * 26 + (name[1] - 'A') * 26 + (name[2] - 'A');
}

long long gcd(long long a, long long b) {
    while (b != 0) {
        long long temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return (a / gcd(a, b)) * b;
}

long long lcmArray(long long arr[], int n) {
    if (n == 0) return 0;
    if (n == 1) return arr[0];

    long long res = arr[0];
    for (int i = 1; i < n; i++) {
        res = lcm(res, arr[i]);
    }
    return res;
}

void parseInput(NSString* filename, char* instructions, Node* nodes, int* nodeMap, int* nodeCount) {
    NSError* error;
    NSString* content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:&error];
    if (!content) {
        NSLog(@"Error reading file: %@", error);
        exit(EXIT_FAILURE);
    }

    NSArray* lines = [content componentsSeparatedByString:@"\n"];

    // Read instructions
    NSString* instrStr = lines[0];
    strcpy(instructions, [instrStr UTF8String]);

    // Initialize node_map
    for (int i = 0; i < HASH_SIZE; ++i) {
        nodeMap[i] = -1;
    }

    // Skip empty line
    for (int i = 2; i < lines.count && *nodeCount < MAX_NODES; i++) {
        NSString* line = lines[i];
        if (line.length < 10) continue;

        NSArray* components = [line componentsSeparatedByString:@" = "];
        if (components.count != 2) continue;

        NSString* nodeName = [components[0] substringToIndex:3];
        NSString* children = components[1];
        children = [children stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@"()"]];
        NSArray* childNodes = [children componentsSeparatedByString:@", "];

        if (childNodes.count != 2) continue;

        strcpy(nodes[*nodeCount].name, [nodeName UTF8String]);
        strcpy(nodes[*nodeCount].left, [[childNodes objectAtIndex:0] UTF8String]);
        strcpy(nodes[*nodeCount].right, [[childNodes objectAtIndex:1] UTF8String]);

        int hashVal = nameToInt(nodes[*nodeCount].name);
        if (hashVal >= 0 && hashVal < HASH_SIZE) {
            nodeMap[hashVal] = *nodeCount;
        } else {
            NSLog(@"Invalid node name format or hash: %s", nodes[*nodeCount].name);
        }
        (*nodeCount)++;
    }
}

long long solve(Node* nodes, int* nodeMap, int nodeCount, char* instructions) {
    int startNodeIndices[MAX_NODES];
    int numStarts = 0;

    for (int i = 0; i < nodeCount; ++i) {
        if (nodes[i].name[NODE_NAME_LEN - 1] == 'A') {
            startNodeIndices[numStarts++] = i;
        }
    }

    if (numStarts == 0) {
        return 0;
    }

    long long steps[MAX_NODES];

    size_t instructionsLength = strlen(instructions);
    for (int i = 0; i < numStarts; ++i) {
        steps[i] = 0;
        int currentNodeIndex = startNodeIndices[i];
        while (nodes[currentNodeIndex].name[NODE_NAME_LEN - 1] != 'Z') {
            char instruction = instructions[steps[i] % instructionsLength];
            const char* nextNodeName;

            if (instruction == 'L') {
                nextNodeName = nodes[currentNodeIndex].left;
            } else {
                nextNodeName = nodes[currentNodeIndex].right;
            }

            int nextHash = nameToInt(nextNodeName);
            if(nextHash < 0 || nextHash >= HASH_SIZE || nodeMap[nextHash] == -1){
                NSLog(@"Error: Node '%s' referenced but not found.", nextNodeName);
                exit(EXIT_FAILURE);
            }
            currentNodeIndex = nodeMap[nextHash];
            steps[i]++;
        }
    }

    return lcmArray(steps, numStarts);
}

int main(int argc, char* argv[]) {
    @autoreleasepool {
        Node nodes[MAX_NODES];
        int nodeMap[HASH_SIZE];
        int nodeCount = 0;
        char instructions[MAX_INSTR_LEN];

        parseInput(@"input.txt", instructions, nodes, nodeMap, &nodeCount);
        long long result = solve(nodes, nodeMap, nodeCount, instructions);
        printf("%lld\n", result);
    }
    return 0;
}
