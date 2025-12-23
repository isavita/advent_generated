
#import <Foundation/Foundation.h>

typedef struct {
    int x, y;
} Coord;

typedef struct {
    int width, height;
    char **data;
} Grid;

Coord add(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

BOOL isInBounds(Grid grid, Coord coord) {
    return coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;
}

Grid parseInput(NSArray *input) {
    NSString *firstLine = input.firstObject;
    Grid grid = {(int)firstLine.length, (int)input.count, malloc(input.count * sizeof(char *))};
    for (int i = 0; i < input.count; i++) {
        NSString *line = input[i];
        grid.data[i] = malloc(grid.width);
        strcpy(grid.data[i], line.UTF8String);
    }
    return grid;
}

Coord findStart(Grid grid) {
    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
            if (grid.data[y][x] == 'S') return (Coord){x, y};
        }
    }
    exit(EXIT_FAILURE);
}

void bfs(Grid grid, Coord start, int *distances) {
    Coord directions[4] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
    int queue[grid.width * grid.height];
    int front = 0, rear = 0;
    queue[rear++] = start.y * grid.width + start.x;
    distances[start.y * grid.width + start.x] = 0;

    while (front < rear) {
        int current = queue[front++];
        Coord currentCoord = {current % grid.width, current / grid.width};

        for (int i = 0; i < 4; i++) {
            Coord neighbor = add(currentCoord, directions[i]);
            if (isInBounds(grid, neighbor) && grid.data[neighbor.y][neighbor.x] != '#') {
                int index = neighbor.y * grid.width + neighbor.x;
                if (distances[index] == -1) {
                    distances[index] = distances[current] + 1;
                    queue[rear++] = index;
                }
            }
        }
    }
}

int solve(Grid grid, int numSteps) {
    int *distances = malloc(grid.width * grid.height * sizeof(int));
    memset(distances, -1, grid.width * grid.height * sizeof(int));
    Coord start = findStart(grid);
    bfs(grid, start, distances);

    int count = 0;
    for (int i = 0; i < grid.width * grid.height; i++) {
        if (distances[i] != -1 && distances[i] <= numSteps && distances[i] % 2 == 0) {
            count++;
        }
    }
    free(distances);
    return count;
}

NSArray *readFile(NSString *fileName) {
    NSError *error;
    NSString *content = [NSString stringWithContentsOfFile:fileName encoding:NSUTF8StringEncoding error:&error];
    if (!content) {
        NSLog(@"Error reading file: %@", error);
        exit(EXIT_FAILURE);
    }
    return [content componentsSeparatedByString:@"\n"];
}

void freeGrid(Grid grid) {
    for (int i = 0; i < grid.height; i++) {
        free(grid.data[i]);
    }
    free(grid.data);
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSArray *input = readFile(@"input.txt");
        Grid grid = parseInput(input);
        printf("%d\n", solve(grid, 64));
        freeGrid(grid);
    }
    return 0;
}
