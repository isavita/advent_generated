import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.conv;

struct Point {
    int x, y;
}

void main() {
    string input = cast(string) read("input.txt");
    auto lines = input.splitter("\n").array;

    char[][] grid;
    Point start, end;

    foreach (ulong i, line; lines) {
        grid ~= line.dup;
        foreach (ulong j, c; line) {
            if (c == 'S') {
                start = Point(cast(int)i, cast(int)j);
                grid[i][j] = 'a';
            } else if (c == 'E') {
                end = Point(cast(int)i, cast(int)j);
                grid[i][j] = 'z';
            }
        }
    }

    writeln(bfs(grid, start, end));
}

int bfs(char[][] grid, Point start, Point end) {
    Point[] directions = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];
    Point[] queue = [start];
    int[][] visited = new int[][](grid.length, grid[0].length);
    visited[start.x][start.y] = 1;

    while (!queue.empty) {
        auto current = queue[0];
        queue = queue[1 .. $];

        foreach (dir; directions) {
            int nx = current.x + dir.x;
            int ny = current.y + dir.y;

            if (nx >= 0 && nx < cast(int)grid.length && ny >= 0 && ny < cast(int)grid[0].length) {
                if (visited[nx][ny] == 0 && grid[nx][ny] <= grid[current.x][current.y] + 1) {
                    visited[nx][ny] = visited[current.x][current.y] + 1;
                    queue ~= Point(nx, ny);

                    if (nx == end.x && ny == end.y) {
                        return visited[nx][ny] - 1;
                    }
                }
            }
        }
    }

    return -1;
}