
import std.stdio;
import std.file;
import std.string;
import std.typecons;

struct Point { int r, c; }

auto calculateRegion(const string[] grid, int r, int c, ref bool[][] visited) {
    auto rows = grid.length;
    auto cols = grid[0].length;
    auto target = grid[r][c];

    long area = 0, perimeter = 0;
    auto queue = [Point(r, c)];
    visited[r][c] = true;
    size_t head = 0;

    while (head < queue.length) {
        auto curr = queue[head++];
        area++;

        int[2][] dirs = [[-1, 0], [1, 0], [0, -1], [0, 1]];
        foreach (dir; dirs) {
            int nr = curr.r + dir[0];
            int nc = curr.c + dir[1];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] == target) {
                    if (!visited[nr][nc]) {
                        visited[nr][nc] = true;
                        queue ~= Point(nr, nc);
                    }
                } else {
                    perimeter++;
                }
            } else {
                perimeter++;
            }
        }
    }
    return tuple(area, perimeter);
}

void main() {
    auto grid = readText("input.txt").strip.splitLines;
    if (grid.empty) {
        writeln(0);
        return;
    }

    auto rows = grid.length;
    auto cols = grid[0].length;
    auto visited = new bool[][](rows, cols);
    long totalPrice = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (!visited[r][c]) {
                auto result = calculateRegion(grid, r, c, visited);
                totalPrice += result[0] * result[1];
            }
        }
    }
    writeln(totalPrice);
}
