
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Challenge: Day 8 Playground
 * 
 * Part 1: Sort all unique pairs of junction boxes by straight-line distance.
 * Process the 1000 shortest pairs, connecting their respective circuits.
 * Multiply together the sizes of the three largest resulting circuits.
 * 
 * Part 2: Continue connecting pairs (shortest first) that are in separate circuits 
 * until all junction boxes form a single large circuit. Multiply the X-coordinates
 * of the two junction boxes in the final merging connection.
 */

// Box structure to store X, Y, Z coordinates and an ID
typedef struct {
    long x, y, z;
    int id;
} Box;

// Edge structure to store the distance between two boxes
typedef struct {
    int u, v;
    long long distSq;
} Edge;

// Union-Find find operation with path compression for efficiency
int findSet(int i, int *parent) {
    int root = i;
    while (parent[root] != root) {
        root = parent[root];
    }
    while (parent[i] != root) {
        int next = parent[i];
        parent[i] = root;
        i = next;
    }
    return root;
}

// Union-Find union operation with union-by-size
void unionSets(int i, int j, int *parent, int *compSize) {
    int rootI = findSet(i, parent);
    int rootJ = findSet(j, parent);
    if (rootI != rootJ) {
        if (compSize[rootI] < compSize[rootJ]) {
            parent[rootI] = rootJ;
            compSize[rootJ] += compSize[rootI];
        } else {
            parent[rootJ] = rootI;
            compSize[rootI] += compSize[rootJ];
        }
    }
}

// Comparator for edges by squared distance, then by indices for stability
int compareEdges(const void *a, const void *b) {
    Edge *e1 = (Edge *)a;
    Edge *e2 = (Edge *)b;
    if (e1->distSq < e2->distSq) return -1;
    if (e1->distSq > e2->distSq) return 1;
    if (e1->u != e2->u) return e1->u - e2->u;
    return e1->v - e2->v;
}

// Comparator for integers in descending order
int compareIntsDesc(const void *a, const void *b) {
    int valA = *(int *)a;
    int valB = *(int *)b;
    if (valA < valB) return 1;
    if (valA > valB) return -1;
    return 0;
}

// Function to calculate the product of 3 largest circuit sizes
long long calculatePart1Answer(int numBoxes, int *parent, int *compSize) {
    int *circuitSizes = (int *)calloc(numBoxes, sizeof(int));
    int count = 0;
    for (int i = 0; i < numBoxes; i++) {
        if (parent[i] == i) {
            circuitSizes[count++] = compSize[i];
        }
    }
    qsort(circuitSizes, count, sizeof(int), compareIntsDesc);
    long long prod = 1;
    for (int i = 0; i < 3 && i < count; i++) {
        prod *= (long long)circuitSizes[i];
    }
    free(circuitSizes);
    return prod;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // 1. Read input from file "input.txt"
        NSString *path = @"input.txt";
        NSError *error;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (!content) {
            content = [NSString stringWithContentsOfFile:@"./input.txt" encoding:NSUTF8StringEncoding error:&error];
        }
        if (!content) {
            fprintf(stderr, "Error: Could not read input.txt\n");
            return 1;
        }

        // 2. Parse junction box coordinates
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        Box *boxes = (Box *)malloc(sizeof(Box) * lines.count);
        int numBoxes = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            if (sscanf([line UTF8String], "%ld,%ld,%ld", &boxes[numBoxes].x, &boxes[numBoxes].y, &boxes[numBoxes].z) == 3) {
                boxes[numBoxes].id = numBoxes;
                numBoxes++;
            }
        }

        if (numBoxes == 0) {
            free(boxes);
            return 0;
        }

        // 3. Generate all unique pairs of junction boxes and compute squared distance
        long long totalPairs = (long long)numBoxes * (numBoxes - 1) / 2;
        Edge *edges = (Edge *)malloc(sizeof(Edge) * totalPairs);
        long long edgeIdx = 0;
        for (int i = 0; i < numBoxes; i++) {
            for (int j = i + 1; j < numBoxes; j++) {
                long long dx = boxes[i].x - boxes[j].x;
                long long dy = boxes[i].y - boxes[j].y;
                long long dz = boxes[i].z - boxes[j].z;
                edges[edgeIdx].u = i;
                edges[edgeIdx].v = j;
                edges[edgeIdx].distSq = dx*dx + dy*dy + dz*dz;
                edgeIdx++;
            }
        }

        // 4. Sort edges by distance (shortest first)
        qsort(edges, totalPairs, sizeof(Edge), compareEdges);

        // 5. Initialize Union-Find structures
        int *parent = (int *)malloc(sizeof(int) * numBoxes);
        int *compSize = (int *)malloc(sizeof(int) * numBoxes);
        for (int i = 0; i < numBoxes; i++) {
            parent[i] = i;
            compSize[i] = 1;
        }

        int numComponents = numBoxes;
        long long part1Ans = 0;
        long long part2Ans = 0;
        BOOL part1Computed = NO;

        // 6. Process sorted edges
        for (long long i = 0; i < totalPairs; i++) {
            int u = edges[i].u;
            int v = edges[i].v;

            // Check if boxes belong to different circuits
            if (findSet(u, parent) != findSet(v, parent)) {
                unionSets(u, v, parent, compSize);
                numComponents--;
                
                // Part 2: capture the last connection that merges everything into one circuit
                if (numComponents == 1 && part2Ans == 0) {
                    part2Ans = (long long)boxes[u].x * (long long)boxes[v].x;
                }
            }

            // Part 1: calculate metrics after precisely 1000 shortest connections
            if (i == 999) {
                part1Ans = calculatePart1Answer(numBoxes, parent, compSize);
                part1Computed = YES;
            }
        }
        
        // Handle case where total possible edges < 1000
        if (!part1Computed) {
            part1Ans = calculatePart1Answer(numBoxes, parent, compSize);
        }

        // 7. Output answers to standard output
        printf("%lld\n", part1Ans);
        printf("%lld\n", part2Ans);

        // Memory cleanup
        free(boxes);
        free(edges);
        free(parent);
        free(compSize);
    }
    return 0;
}

