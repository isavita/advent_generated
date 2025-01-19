
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 200
#define MAX_LINE_LENGTH 200

typedef struct {
    int x;
    int y;
} Coord;

typedef unsigned char Tile;

typedef struct {
    bool top;
    bool right;
    bool bottom;
    bool left;
} Pipe;

typedef struct {
    int width;
    int height;
    Tile data[MAX_GRID_SIZE][MAX_GRID_SIZE];
} Grid;

Coord Undefined = {0, 0};
Coord Top = {0, -1};
Coord Right = {1, 0};
Coord Bottom = {0, 1};
Coord Left = {-1, 0};

const Tile Empty = '.';
const Tile Start = 'S';
const Tile Vertical = '|';
const Tile Horizontal = '-';
const Tile TopLeftCorner = 'J';
const Tile TopRightCorner = 'L';
const Tile BottomLeftCorner = '7';
const Tile BottomRightCorner = 'F';
const Tile Enclosed = 'X';


Pipe VerticalPipe = {true, false, true, false};
Pipe HorizontalPipe = {false, true, false, true};
Pipe TopLeftCornerPipe = {true, false, false, true};
Pipe TopRightCornerPipe = {true, true, false, false};
Pipe BottomLeftCornerPipe = {false, false, true, true};
Pipe BottomRightCornerPipe = {false, true, true, false};


Pipe getPipeFromTile(Tile tile) {
    switch (tile) {
        case Vertical:          return VerticalPipe;
        case Horizontal:        return HorizontalPipe;
        case TopLeftCorner:     return TopLeftCornerPipe;
        case TopRightCorner:    return TopRightCornerPipe;
        case BottomLeftCorner:  return BottomLeftCornerPipe;
        case BottomRightCorner: return BottomRightCornerPipe;
        default: {Pipe p = {false, false, false, false}; return p;}
    }
}


Tile getTileFromPipe(Pipe pipe) {
    if (memcmp(&pipe, &VerticalPipe, sizeof(Pipe)) == 0) return Vertical;
    if (memcmp(&pipe, &HorizontalPipe, sizeof(Pipe)) == 0) return Horizontal;
    if (memcmp(&pipe, &TopLeftCornerPipe, sizeof(Pipe)) == 0) return TopLeftCorner;
    if (memcmp(&pipe, &TopRightCornerPipe, sizeof(Pipe)) == 0) return TopRightCorner;
    if (memcmp(&pipe, &BottomLeftCornerPipe, sizeof(Pipe)) == 0) return BottomLeftCorner;
    if (memcmp(&pipe, &BottomRightCornerPipe, sizeof(Pipe)) == 0) return BottomRightCorner;
    return Empty;
}


Grid buildGrid(char *input[], int lines) {
    Grid grid;
    grid.width = strlen(input[0]);
    grid.height = lines;
    for(int y=0; y < lines; y++){
         for(int x=0; x < grid.width; x++){
             grid.data[y][x] = Empty;
            if(input[y][x] != Empty){
                 grid.data[y][x] = input[y][x];
            }
         }
    }
    return grid;
}

Coord findStart(Grid grid) {
    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
            if (grid.data[y][x] == Start) {
                Coord c = {x, y};
                return c;
            }
        }
    }
    return Undefined;
}

Pipe getPipeFromNeighbors(Coord c, Grid grid) {
    Pipe pipe = {false, false, false, false};
    Coord possibleNeighbors[4] = {
        {c.x, c.y - 1},
        {c.x + 1, c.y},
        {c.x, c.y + 1},
        {c.x - 1, c.y}
    };
    Coord directions[4] = {Top, Right, Bottom, Left};

    for(int i=0; i<4; i++){
         Coord neighborCoord = possibleNeighbors[i];
         Coord dir = directions[i];
           if (neighborCoord.x >= 0 && neighborCoord.x < grid.width &&
                neighborCoord.y >= 0 && neighborCoord.y < grid.height) {
                Pipe neighborPipe = getPipeFromTile(grid.data[neighborCoord.y][neighborCoord.x]);
                if( (dir.x == Top.x && dir.y == Top.y && neighborPipe.bottom) ||
                    (dir.x == Right.x && dir.y == Right.y && neighborPipe.left) ||
                    (dir.x == Bottom.x && dir.y == Bottom.y && neighborPipe.top) ||
                    (dir.x == Left.x && dir.y == Left.y && neighborPipe.right)){
                         if(dir.x == Top.x && dir.y == Top.y){ pipe.top = true; }
                         if(dir.x == Right.x && dir.y == Right.y){ pipe.right = true; }
                         if(dir.x == Bottom.x && dir.y == Bottom.y){ pipe.bottom = true; }
                         if(dir.x == Left.x && dir.y == Left.y){ pipe.left = true; }
                     }
             }
    }
    return pipe;
}


int pathFinding(Coord start, Grid grid, Coord* path) {
    path[0] = start;
    Pipe startPipe = getPipeFromNeighbors(start, grid);

    Coord previousDir;
    Coord current;
    int pathLength = 1;

    if(startPipe.top) { previousDir = Top; current.x = start.x + Top.x; current.y = start.y + Top.y; }
    else if(startPipe.right) { previousDir = Right; current.x = start.x + Right.x; current.y = start.y + Right.y; }
    else if(startPipe.bottom) { previousDir = Bottom; current.x = start.x + Bottom.x; current.y = start.y + Bottom.y; }
     else if(startPipe.left) { previousDir = Left; current.x = start.x + Left.x; current.y = start.y + Left.y; }


    while (current.x != start.x || current.y != start.y) {
          path[pathLength] = current;
           pathLength++;
        Pipe currentPipe = getPipeFromTile(grid.data[current.y][current.x]);
        if(currentPipe.top && !(previousDir.x == Bottom.x && previousDir.y == Bottom.y)) {
            previousDir = Top; current.x += Top.x; current.y += Top.y;
        } else if(currentPipe.right && !(previousDir.x == Left.x && previousDir.y == Left.y)){
              previousDir = Right; current.x += Right.x; current.y += Right.y;
        } else if(currentPipe.bottom && !(previousDir.x == Top.x && previousDir.y == Top.y)){
             previousDir = Bottom; current.x += Bottom.x; current.y += Bottom.y;
        } else if(currentPipe.left && !(previousDir.x == Right.x && previousDir.y == Right.y)){
              previousDir = Left; current.x += Left.x; current.y += Left.y;
        }
    }
    return pathLength;
}


void getPathGrid(Grid grid, Coord* path, int pathLength, Tile empty, Grid *newGrid) {
    newGrid->width = grid.width;
    newGrid->height = grid.height;
    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
             newGrid->data[y][x] = empty;
        }
    }
     for (int i = 0; i < pathLength; i++) {
        Coord coord = path[i];
        newGrid->data[coord.y][coord.x] = grid.data[coord.y][coord.x];
    }
    Coord start = path[0];
    Pipe startPipe = getPipeFromNeighbors(start, grid);
     newGrid->data[start.y][start.x] = getTileFromPipe(startPipe);
}


bool isInside(Coord c, Grid grid, Tile empty) {
    if (grid.data[c.y][c.x] != empty) {
        return false;
    }

    Tile startPipe = empty;
    int numPipeOnLeft = 0;
    for (int x = 0; x < c.x; x++) {
        Tile v = grid.data[c.y][x];
         switch (v) {
             case Vertical:
                 numPipeOnLeft++;
                 break;
             case TopRightCorner:
                 startPipe = TopRightCorner;
                 break;
             case BottomRightCorner:
                 startPipe = BottomRightCorner;
                 break;
             case TopLeftCorner:
                 if (startPipe == BottomRightCorner) {
                     startPipe = empty;
                     numPipeOnLeft++;
                 } else if (v == TopRightCorner) {
                     startPipe = empty;
                 }
                 break;
             case BottomLeftCorner:
                 if (startPipe == TopRightCorner) {
                     startPipe = empty;
                     numPipeOnLeft++;
                 } else if (startPipe == BottomRightCorner) {
                     startPipe = empty;
                 }
                 break;
         }
    }

    return numPipeOnLeft % 2 == 1;
}


int solve(char *input[], int lines) {
    Grid grid = buildGrid(input, lines);
    Coord start = findStart(grid);
    Coord path[MAX_GRID_SIZE*MAX_GRID_SIZE];
    int pathLength = pathFinding(start, grid, path);
    return pathLength / 2;
}

int main() {
    FILE *fp;
    char *input[MAX_GRID_SIZE];
    char line[MAX_LINE_LENGTH];
    int lines = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    while (fgets(line, sizeof(line), fp) != NULL && lines < MAX_GRID_SIZE) {
        input[lines] = strdup(line);
        if (input[lines] == NULL) {
            perror("Memory allocation failed");
           fclose(fp);
            for (int i = 0; i < lines; i++) {
                free(input[i]);
            }
            return 1;
        }
        input[lines][strcspn(input[lines], "\n")] = 0;
        lines++;
    }

    fclose(fp);

    int result = solve(input, lines);
    printf("%d\n", result);

    for (int i = 0; i < lines; i++) {
        free(input[i]);
    }

    return 0;
}
