
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_H 1005
#define MAX_W 1005
#define MAX_QUEUE_SIZE (MAX_H * MAX_W)
#define MAX_STEPS 20
#define INF -1

typedef struct {
    int r, c;
} Point;

char grid[MAX_H][MAX_W];
int walls[MAX_H][MAX_W];
int dist_s[MAX_H][MAX_W];
int dist_e[MAX_H][MAX_W];
int dist_c[MAX_H][MAX_W];
int H, W;
Point S, E;

Point queue[MAX_QUEUE_SIZE];
int q_head, q_tail;

int dr[] = {1, -1, 0, 0};
int dc[] = {0, 0, 1, -1};

void q_reset() {
    q_head = q_tail = 0;
}

void q_push(Point p) {
    queue[q_tail++] = p;
}

Point q_pop() {
    return queue[q_head++];
}

int q_empty() {
    return q_head == q_tail;
}

int is_valid(int r, int c) {
    return r >= 0 && r < H && c >= 0 && c < W && !walls[r][c];
}

void bfs(Point start, int dist[][MAX_W]) {
    memset(dist, INF, sizeof(dist_s)); // Use sizeof one of the dist arrays

    if (!is_valid(start.r, start.c)) return;

    dist[start.r][start.c] = 0;
    q_reset();
    q_push(start);

    while (!q_empty()) {
        Point curr = q_pop();
        int d = dist[curr.r][curr.c];

        for (int i = 0; i < 4; ++i) {
            int nr = curr.r + dr[i];
            int nc = curr.c + dc[i];

            if (is_valid(nr, nc) && dist[nr][nc] == INF) {
                dist[nr][nc] = d + 1;
                Point next = {nr, nc};
                q_push(next);
            }
        }
    }
}

void limited_bfs(Point start, int max_steps) {
     memset(dist_c, INF, sizeof(dist_c));

    if (!is_valid(start.r, start.c)) return;

    dist_c[start.r][start.c] = 0;
    q_reset();
    q_push(start);

    while (!q_empty()) {
        Point curr = q_pop();
        int d = dist_c[curr.r][curr.c];

        if (d >= max_steps) continue; // Stop exploring if max steps reached

        for (int i = 0; i < 4; ++i) {
            int nr = curr.r + dr[i];
            int nc = curr.c + dc[i];

            // Check bounds only (walls checked by is_valid later)
             if (nr >= 0 && nr < H && nc >= 0 && nc < W && dist_c[nr][nc] == INF) {
                 // Allow moving into walls during limited BFS but mark distance
                 // The validity check (is_track) happens later when calculating cost
                 dist_c[nr][nc] = d + 1;
                 Point next = {nr, nc};
                 q_push(next);
            }
        }
    }
}


int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        // fprintf(stderr, "Error opening input.txt\n"); // No explanation allowed
        return 1;
    }

    H = 0;
    while (fgets(grid[H], MAX_W, f) != NULL) {
        // Remove trailing newline if present
        grid[H][strcspn(grid[H], "\n")] = 0;
        if (H == 0) {
            W = strlen(grid[0]);
        }
        for (int j = 0; j < W; ++j) {
            if (grid[H][j] == 'S') {
                S.r = H; S.c = j;
            } else if (grid[H][j] == 'E') {
                E.r = H; E.c = j;
            }
            if (grid[H][j] == '#') {
                walls[H][j] = 1;
            } else {
                walls[H][j] = 0;
            }
        }
        H++;
    }
    fclose(f);

    bfs(S, dist_s);
    bfs(E, dist_e);

    int normal_cost = dist_s[E.r][E.c];

    if (normal_cost == INF) {
        printf("0\n");
        return 0;
    }

    long long cheat_count = 0;

    for (int sr = 0; sr < H; ++sr) {
        for (int sc = 0; sc < W; ++sc) {
            // Start point must be reachable from S and not a wall
            if (walls[sr][sc] || dist_s[sr][sc] == INF) {
                continue;
            }
            int sd = dist_s[sr][sc];
            Point start_cheat = {sr, sc};
            limited_bfs(start_cheat, MAX_STEPS);

            for (int er = 0; er < H; ++er) {
                for (int ec = 0; ec < W; ++ec) {
                    // End point must be reachable from E and not a wall
                    if (walls[er][ec] || dist_e[er][ec] == INF) {
                        continue;
                    }

                    int s = dist_c[er][ec]; // Steps for the cheat segment

                    // Check if reachable within limits (0 < s <= 20)
                    if (s > 0 && s <= MAX_STEPS) {
                        int ed = dist_e[er][ec];
                        int total_cost = sd + s + ed;

                        if (total_cost < normal_cost && (normal_cost - total_cost) >= 100) {
                            cheat_count++;
                        }
                    }
                }
            }
        }
    }

    printf("%lld\n", cheat_count);

    return 0;
}
