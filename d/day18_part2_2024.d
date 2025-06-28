
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.container;

static immutable int GRID_SIZE = 71;

struct Point {
    int x, y;
}

struct BFSState {
    int r, c, dist;
}

bool isValid(int r, int c, ref char[GRID_SIZE][GRID_SIZE] grid, ref bool[GRID_SIZE][GRID_SIZE] visited) {
    return r >= 0 && r < GRID_SIZE && c >= 0 && c < GRID_SIZE &&
           grid[r][c] == '.' && !visited[r][c];
}

int bfs(ref char[GRID_SIZE][GRID_SIZE] grid) {
    auto q = DList!BFSState();
    bool[GRID_SIZE][GRID_SIZE] visited;

    q.insertBack(BFSState(0, 0, 0));
    visited[0][0] = true;

    static immutable int[] DR = [0, 0, 1, -1];
    static immutable int[] DC = [1, -1, 0, 0];

    while (!q.empty) {
        BFSState current = q.front;
        q.removeFront();

        if (current.r == GRID_SIZE - 1 && current.c == GRID_SIZE - 1) {
            return current.dist;
        }

        foreach (i; 0..4) {
            int nr = current.r + DR[i];
            int nc = current.c + DC[i];

            if (isValid(nr, nc, grid, visited)) {
                visited[nr][nc] = true;
                q.insertBack(BFSState(nr, nc, current.dist + 1));
            }
        }
    }
    return -1;
}

void main() {
    string[] lines = readText("input.txt").strip().splitLines();
    Point[] bytePositions;
    foreach (line; lines) {
        string[] parts = line.split(",");
        bytePositions ~= Point(parts[0].to!int, parts[1].to!int);
    }

    char[GRID_SIZE][GRID_SIZE] gridPart1;
    foreach (r; 0..GRID_SIZE) {
        foreach (c; 0..GRID_SIZE) {
            gridPart1[r][c] = '.';
        }
    }

    foreach (i, pos; bytePositions[0..1024]) {
        gridPart1[pos.y][pos.x] = '#';
    }
    
    int part1Result = bfs(gridPart1);
    writeln(part1Result);

    char[GRID_SIZE][GRID_SIZE] gridPart2;
    foreach (r; 0..GRID_SIZE) {
        foreach (c; 0..GRID_SIZE) {
            gridPart2[r][c] = '.';
        }
    }

    foreach (i, pos; bytePositions) {
        gridPart2[pos.y][pos.x] = '#';
        
        if (bfs(gridPart2) == -1) {
            writefln("%s,%s", pos.x, pos.y);
            break;
        }
    }
}
