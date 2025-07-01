
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.container.dlist;
import std.algorithm;
import std.range;

struct Point { int r, c; }

int[][] normalBfs(Point start, int h, int w, bool[][] walls) {
    auto dist = new int[][](h, w);
    foreach (row; dist) row[] = -1;
    auto q = DList!Point(start);
    dist[start.r][start.c] = 0;
    immutable Point[] dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];

    while (!q.empty) {
        auto curr = q.front;
        q.removeFront();
        foreach (d; dirs) {
            int nr = curr.r + d.r, nc = curr.c + d.c;
            if (nr >= 0 && nr < h && nc >= 0 && nc < w && !walls[nr][nc] && dist[nr][nc] == -1) {
                dist[nr][nc] = dist[curr.r][curr.c] + 1;
                q.insertBack(Point(nr, nc));
            }
        }
    }
    return dist;
}

int[][] cheatBfs(Point start, int h, int w) {
    auto dist = new int[][](h, w);
    foreach (row; dist) row[] = -1;
    auto q = DList!Point(start);
    dist[start.r][start.c] = 0;
    immutable Point[] dirs = [Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1)];

    while (!q.empty) {
        auto curr = q.front;
        q.removeFront();
        auto steps = dist[curr.r][curr.c];
        if (steps == 20) continue;
        foreach (d; dirs) {
            int nr = curr.r + d.r, nc = curr.c + d.c;
            if (nr >= 0 && nr < h && nc >= 0 && nc < w && dist[nr][nc] == -1) {
                dist[nr][nc] = steps + 1;
                q.insertBack(Point(nr, nc));
            }
        }
    }
    return dist;
}

void main() {
    auto input = readText("input.txt").strip.split("\n");
    auto h = cast(int)input.length;
    auto w = cast(int)input[0].length;

    Point S, E;
    auto walls = new bool[][](h, w);
    Point[] trackCells;

    foreach (r, line; input) {
        foreach (c, ch; line) {
            if (ch == 'S') S = Point(cast(int)r, cast(int)c);
            else if (ch == 'E') E = Point(cast(int)r, cast(int)c);
            
            if (ch == '#') {
                walls[r][c] = true;
            } else {
                trackCells ~= Point(cast(int)r, cast(int)c);
            }
        }
    }

    auto distFromS = normalBfs(S, h, w, walls);
    auto distFromE = normalBfs(E, h, w, walls);

    auto normalCost = distFromS[E.r][E.c];
    if (normalCost == -1) {
        writeln(0);
        return;
    }

    long[string] cheats;

    foreach (startPos; trackCells) {
        auto sd = distFromS[startPos.r][startPos.c];
        if (sd == -1) continue;

        auto distC = cheatBfs(startPos, h, w);

        foreach (r; 0 .. h) {
            foreach (c; 0 .. w) {
                auto s = distC[r][c];
                if (s > 0 && s <= 20 && !walls[r][c]) {
                    auto ed = distFromE[r][c];
                    if (ed == -1) continue;

                    auto cost = sd + s + ed;
                    if (cost < normalCost) {
                        auto key = format("%s,%s,%s,%s", startPos.r, startPos.c, r, c);
                        if (key !in cheats || cost < cheats[key]) {
                            cheats[key] = cost;
                        }
                    }
                }
            }
        }
    }

    writeln(cheats.values.count!(cost => normalCost - cost >= 100));
}
