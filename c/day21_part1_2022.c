
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

typedef struct Node {
    char *name;
    char *job;
    int value;
    bool calculated;
} Node;

typedef struct {
    Node *nodes;
    int capacity;
    int count;
} NodeMap;

NodeMap node_map_init() {
    NodeMap map;
    map.capacity = 16;
    map.count = 0;
    map.nodes = malloc(sizeof(Node) * map.capacity);
    return map;
}

void node_map_resize(NodeMap *map) {
    map->capacity *= 2;
    map->nodes = realloc(map->nodes, sizeof(Node) * map->capacity);
}

void node_map_put(NodeMap *map, char *name, char *job) {
    if (map->count == map->capacity) {
        node_map_resize(map);
    }
    Node node;
    node.name = strdup(name);
    node.job = strdup(job);
    node.calculated = false;
    map->nodes[map->count++] = node;
}

Node* node_map_get(NodeMap *map, char *name) {
    for (int i = 0; i < map->count; i++) {
        if (strcmp(map->nodes[i].name, name) == 0) {
            return &map->nodes[i];
        }
    }
    return NULL;
}


long calculate(char *monkey, NodeMap *jobs) {
    Node *node = node_map_get(jobs, monkey);
    if (node == NULL) {
        fprintf(stderr, "Monkey not found: %s\n", monkey);
        exit(1);
    }

    if(node->calculated){
        return node->value;
    }

    char *job = node->job;

    int num = 0;
    bool is_num = true;
    for(int i=0; job[i] != '\0'; i++){
        if(!isdigit(job[i])){
            is_num = false;
            break;
        }
        num = num * 10 + (job[i] - '0');
    }
    
    if (is_num) {
         node->value = num;
         node->calculated = true;
         return num;
    }
    

    char part1[10], op[2], part2[10];
    sscanf(job, "%s %s %s", part1, op, part2);
    
    long a = calculate(part1, jobs);
    long b = calculate(part2, jobs);
    long result;

    switch (op[0]) {
        case '+':
            result = a + b;
            break;
        case '-':
            result = a - b;
            break;
        case '*':
            result = a * b;
            break;
        case '/':
            result = a / b;
            break;
        default:
            fprintf(stderr, "Unknown operation: %s\n", op);
            exit(1);
    }
    node->value = result;
    node->calculated = true;
    return result;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    NodeMap jobs = node_map_init();
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        char *colon = strchr(line, ':');
        if (colon == NULL) continue;

        *colon = '\0';
        char *name = line;
        char *job = colon + 2;
        
        size_t job_len = strlen(job);
        if (job_len > 0 && job[job_len-1] == '\n')
            job[job_len-1] = '\0';
        
        node_map_put(&jobs, name, job);
    }
    free(line);
    fclose(file);

    printf("%ld\n", calculate("root", &jobs));

    for (int i = 0; i < jobs.count; i++) {
         free(jobs.nodes[i].name);
         free(jobs.nodes[i].job);
    }
    free(jobs.nodes);

    return 0;
}
