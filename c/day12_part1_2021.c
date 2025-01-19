
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_CAVES 20
#define MAX_NAME_LEN 10

typedef struct {
    char name[MAX_NAME_LEN];
    int connections[MAX_CAVES];
    int num_connections;
} Cave;

Cave caves[MAX_CAVES];
int num_caves = 0;
int cave_map[256];
int num_paths = 0;


int get_cave_index(char *name) {
    int i;
    for (i = 0; i < num_caves; i++) {
        if (strcmp(caves[i].name, name) == 0) {
            return i;
        }
    }
    strcpy(caves[num_caves].name, name);
    caves[num_caves].num_connections = 0;
    cave_map[name[0]] = num_caves;
    return num_caves++;
}

void connect_caves(int from, int to) {
    caves[from].connections[caves[from].num_connections++] = to;
}

bool is_small(char *name) {
    return islower(name[0]);
}

void dfs(int current, bool *visited) {
    if (strcmp(caves[current].name, "end") == 0) {
        num_paths++;
        return;
    }

    for (int i = 0; i < caves[current].num_connections; i++) {
        int next = caves[current].connections[i];
        if (visited[next] && is_small(caves[next].name)) {
            continue;
        }
        visited[next] = true;
        dfs(next, visited);
        visited[next] = false;
    }
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[50];
    while (fgets(line, sizeof(line), file)) {
        char *from_name = strtok(line, "-");
        char *to_name = strtok(NULL, "\n");

        int from = get_cave_index(from_name);
        int to = get_cave_index(to_name);
        connect_caves(from, to);
        connect_caves(to, from);
    }
    fclose(file);
    bool visited[MAX_CAVES] = {false};
    visited[get_cave_index("start")] = true;
    dfs(get_cave_index("start"), visited);
    printf("%d\n", num_paths);
    return 0;
}
