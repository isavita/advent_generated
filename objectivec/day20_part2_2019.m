
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_H 150
#define MAX_W 150
#define MAX_LEVEL 50
#define MAX_PORTALS 100
#define MAX_QUEUE_SIZE 1000000

typedef struct { int x, y; } Point;
typedef struct { int x, y, level, steps; } State;
typedef struct { Point from; Point to; bool is_outer; } PortalLink;
typedef struct { char label[3]; Point pos; } FoundPortal;

char grid[MAX_H][MAX_W];
int height = 0, width = 0;
Point start, end;
PortalLink portal_map[MAX_PORTALS];
int portal_count = 0;
State queue[MAX_QUEUE_SIZE];
int q_head = 0, q_tail = 0;
bool visited[MAX_H][MAX_W][MAX_LEVEL];

bool is_outer(Point p) {
    return p.x <= 2 || p.y <= 2 || p.x >= width - 3 || p.y >= height - 3;
}

PortalLink* get_portal_link(Point p) {
    for (int i = 0; i < portal_count; ++i) {
        if (portal_map[i].from.x == p.x && portal_map[i].from.y == p.y) {
            return &portal_map[i];
        }
    }
    return NULL;
}

void read_input() {
    FILE *f = fopen("input.txt", "r");
    if (!f) { perror("Error opening file"); exit(1); }
    width = 0;
    height = 0;
    while (fgets(grid[height], MAX_W, f)) {
        int len = strlen(grid[height]);
        if (len > 0 && grid[height][len - 1] == '\n') {
            grid[height][len - 1] = '\0';
            len--;
        }
        if (len > width) {
            width = len;
        }
        height++;
        if (height >= MAX_H) break;
    }
    fclose(f);
    for(int y=0; y<height; ++y) {
        int len = strlen(grid[y]);
        for(int x=len; x<width; ++x) {
            grid[y][x] = ' ';
        }
        grid[y][width] = '\0';
    }
}

void find_and_map_portals() {
    FoundPortal found[200];
    int found_count = 0;

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            if (isupper(grid[y][x])) {
                char label[3] = {0};
                Point pos = {-1, -1};

                if (x + 1 < width && isupper(grid[y][x+1])) {
                    label[0] = grid[y][x];
                    label[1] = grid[y][x+1];
                    if (x > 0 && grid[y][x-1] == '.') pos = (Point){x-1, y};
                    else if (x + 2 < width && grid[y][x+2] == '.') pos = (Point){x+2, y};
                } else if (y + 1 < height && isupper(grid[y+1][x])) {
                    label[0] = grid[y][x];
                    label[1] = grid[y+1][x];
                    if (y > 0 && grid[y-1][x] == '.') pos = (Point){x, y-1};
                    else if (y + 2 < height && grid[y+2][x] == '.') pos = (Point){x, y+2};
                }

                if (pos.x != -1) {
                    strcpy(found[found_count].label, label);
                    found[found_count].pos = pos;
                    found_count++;
                }
            }
        }
    }

    for (int i = 0; i < found_count; ++i) {
        if (strcmp(found[i].label, "AA") == 0) {
            start = found[i].pos;
        } else if (strcmp(found[i].label, "ZZ") == 0) {
            end = found[i].pos;
        } else {
            for (int j = i + 1; j < found_count; ++j) {
                if (strcmp(found[i].label, found[j].label) == 0) {
                    bool i_outer = is_outer(found[i].pos);
                    bool j_outer = is_outer(found[j].pos);
                    portal_map[portal_count++] = (PortalLink){found[i].pos, found[j].pos, i_outer};
                    portal_map[portal_count++] = (PortalLink){found[j].pos, found[i].pos, j_outer};
                    found[j].label[0] = '\0';
                    break;
                }
            }
        }
    }
}

int bfs() {
    memset(visited, 0, sizeof(visited));
    q_head = q_tail = 0;

    queue[q_tail++] = (State){start.x, start.y, 0, 0};
    visited[start.y][start.x][0] = true;

    int dx[] = {0, 0, 1, -1};
    int dy[] = {1, -1, 0, 0};

    while (q_head != q_tail) {
        State current = queue[q_head++];
        q_head %= MAX_QUEUE_SIZE;

        if (current.x == end.x && current.y == end.y && current.level == 0) {
            return current.steps;
        }

        for (int i = 0; i < 4; ++i) {
            int nx = current.x + dx[i];
            int ny = current.y + dy[i];

            if (nx >= 0 && nx < width && ny >= 0 && ny < height && grid[ny][nx] == '.') {
                if (current.level < MAX_LEVEL && !visited[ny][nx][current.level]) {
                    visited[ny][nx][current.level] = true;
                    queue[q_tail++] = (State){nx, ny, current.level, current.steps + 1};
                    q_tail %= MAX_QUEUE_SIZE;
                    if (q_tail == q_head) { fprintf(stderr, "Queue overflow\n"); exit(1); }
                }
            }
        }

        PortalLink* link = get_portal_link((Point){current.x, current.y});
        if (link) {
            int new_level = current.level + (link->is_outer ? -1 : 1);
            if (new_level >= 0 && new_level < MAX_LEVEL) {
                Point target = link->to;
                if (!visited[target.y][target.x][new_level]) {
                    visited[target.y][target.x][new_level] = true;
                    queue[q_tail++] = (State){target.x, target.y, new_level, current.steps + 1};
                    q_tail %= MAX_QUEUE_SIZE;
                    if (q_tail == q_head) { fprintf(stderr, "Queue overflow\n"); exit(1); }
                }
            }
        }
    }

    return -1;
}

int main(int argc, const char * argv[]) {
    read_input();
    find_and_map_portals();
    int result = bfs();
    printf("%d\n", result);
    return 0;
}
