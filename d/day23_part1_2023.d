
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.array;

struct Point { int r, c; }

int maxSteps;
string[] grid;
Point end;
bool[][] visited;

void dfs(Point p, int steps) {
    if (p == end) {
        maxSteps = max(maxSteps, steps);
        return;
    }

    int r = p.r, c = p.c;

    void tryMove(int dr, int dc) {
        int nr = r + dr, nc = c + dc;
        if (nr < 0 || nr >= grid.length || nc < 0 || nc >= grid[0].length ||
            grid[nr][nc] == '#' || visited[nr][nc]) {
            return;
        }
        visited[nr][nc] = true;
        dfs(Point(nr, nc), steps + 1);
        visited[nr][nc] = false;
    }

    switch (grid[r][c]) {
        case '.':
            tryMove(-1, 0);
            tryMove(1, 0);
            tryMove(0, -1);
            tryMove(0, 1);
            break;
        case '^': tryMove(-1, 0); break;
        case 'v': tryMove(1, 0); break;
        case '<': tryMove(0, -1); break;
        case '>': tryMove(0, 1); break;
        default: break;
    }
}

void main() {
    grid = readText("input.txt").strip.splitLines;
    auto height = grid.length;
    auto width = grid[0].length;

    auto start = Point(0, cast(int)grid[0].indexOf('.'));
    end = Point(cast(int)height - 1, cast(int)grid[$-1].indexOf('.'));

    visited = new bool[][](height, width);
    visited[start.r][start.c] = true;

    dfs(start, 0);

    writeln(maxSteps);
}
