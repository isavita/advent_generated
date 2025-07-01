
import std.stdio;
import std.file;
import std.string;
import std.array;
import std.algorithm;

struct Point { int r, c; }

int[][] bfs(Point start, size_t h, size_t w, const bool[][] walls) {
    auto dist = new int[][](h, w);
    foreach (ref row; dist) {
        row[] = -1;
    }

    auto q = [start];
    dist[start.r][start.c] = 0;
    size_t head = 0;

    const dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];

    while (head < q.length) {
        auto curr = q[head++];
        foreach (d; dirs) {
            int nr = curr.r + d.r;
            int nc = curr.c + d.c;

            if (nr < 0 || nr >= h || nc < 0 || nc >= w || walls[nr][nc] || dist[nr][nc] != -1) {
                continue;
            }
            dist[nr][nc] = dist[curr.r][curr.c] + 1;
            q ~= Point(nr, nc);
        }
    }
    return dist;
}

void main() {
    auto input = readText("input.txt").strip.splitLines;
    auto h = input.length;
    auto w = input[0].length;

    Point S, E;
    auto walls = new bool[][](h, w);
    Point[] trackCells;
    trackCells.reserve(h * w);

    foreach (r, line; input) {
        foreach (c, ch; line) {
            final switch (ch) {
                case 'S': S = Point(cast(int)r, cast(int)c); break;
                case 'E': E = Point(cast(int)r, cast(int)c); break;
                case '#': walls[r][c] = true; continue;
                case '.': break;
            }
            trackCells ~= Point(cast(int)r, cast(int)c);
        }
    }

    auto distFromS = bfs(S, h, w, walls);
    auto distFromE = bfs(E, h, w, walls);

    auto normalCost = distFromS[E.r][E.c];
    if (normalCost == -1) {
        writeln(0);
        return;
    }

    const dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];
    long possibleCheats = 0;

    foreach (startPos; trackCells) {
        int sd = distFromS[startPos.r][startPos.c];
        if (sd == -1) continue;

        foreach (d1; dirs) {
            int m1r = startPos.r + d1.r;
            int m1c = startPos.c + d1.c;
            if (m1r < 0 || m1r >= h || m1c < 0 || m1c >= w) continue;

            foreach (d2; dirs) {
                int m2r = m1r + d2.r;
                int m2c = m1c + d2.c;
                if (m2r < 0 || m2r >= h || m2c < 0 || m2c >= w || walls[m2r][m2c]) continue;
                
                int ed = distFromE[m2r][m2c];
                if (ed == -1) continue;

                if (normalCost - (sd + 2 + ed) >= 100) {
                    possibleCheats++;
                }
            }
        }
    }

    writeln(possibleCheats);
}
