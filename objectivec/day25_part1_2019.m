
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define INITIAL_MEM_CAP 8192
#define INITIAL_INPUT_CAP 256
#define MAX_OUTPUT_BUF 16384
#define MAX_LINES 256
#define MAP_CAPACITY 256
#define PARSE_BUF_SIZE 256

typedef struct MapNode {
    char* key;
    void* value;
    struct MapNode* next;
} MapNode;

typedef struct {
    MapNode** buckets;
    int capacity;
} Map;

typedef struct Room {
    char* name;
    Map* connections;
} Room;

typedef struct {
    long long* mem;
    long long mem_cap;
    long long ip;
    long long rel_base;
    long long* input_q;
    int input_head;
    int input_tail;
    int input_cap;
} Emulator;

enum Status { HALTED, OUTPUT, WAITING };
enum Mode { EXPLORE, NAVIGATE, TEST };

static unsigned long hash_djb2(const char* str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++)) hash = ((hash << 5) + hash) + c;
    return hash;
}

static Map* map_create() {
    Map* map = malloc(sizeof(Map));
    map->capacity = MAP_CAPACITY;
    map->buckets = calloc(map->capacity, sizeof(MapNode*));
    return map;
}

static void map_put(Map* map, const char* key, void* value) {
    unsigned long h = hash_djb2(key) % map->capacity;
    MapNode* node = map->buckets[h];
    while (node) {
        if (strcmp(node->key, key) == 0) {
            node->value = value;
            return;
        }
        node = node->next;
    }
    node = malloc(sizeof(MapNode));
    node->key = strdup(key);
    node->value = value;
    node->next = map->buckets[h];
    map->buckets[h] = node;
}

static void* map_get(Map* map, const char* key) {
    unsigned long h = hash_djb2(key) % map->capacity;
    MapNode* node = map->buckets[h];
    while (node) {
        if (strcmp(node->key, key) == 0) return node->value;
        node = node->next;
    }
    return NULL;
}

static void map_destroy(Map* map, void (*free_value)(void*)) {
    for (int i = 0; i < map->capacity; i++) {
        MapNode* node = map->buckets[i];
        while (node) {
            MapNode* temp = node;
            node = node->next;
            free(temp->key);
            if (free_value && temp->value) free_value(temp->value);
            free(temp);
        }
    }
    free(map->buckets);
    free(map);
}

static void ensure_mem(Emulator* emu, long long addr) {
    if (addr >= emu->mem_cap) {
        long long old_cap = emu->mem_cap;
        while (addr >= emu->mem_cap) emu->mem_cap *= 2;
        emu->mem = realloc(emu->mem, emu->mem_cap * sizeof(long long));
        memset(emu->mem + old_cap, 0, (emu->mem_cap - old_cap) * sizeof(long long));
    }
}

static long long get_param_val(Emulator* emu, int offset) {
    long long instruction = emu->mem[emu->ip];
    int mode = (instruction / (offset == 1 ? 100 : (offset == 2 ? 1000 : 10000))) % 10;
    long long param = emu->mem[emu->ip + offset];
    if (mode == 0) { ensure_mem(emu, param); return emu->mem[param]; }
    if (mode == 1) return param;
    if (mode == 2) { ensure_mem(emu, emu->rel_base + param); return emu->mem[emu->rel_base + param]; }
    return 0;
}

static long long get_write_addr(Emulator* emu, int offset) {
    long long instruction = emu->mem[emu->ip];
    int mode = (instruction / (offset == 1 ? 100 : (offset == 2 ? 1000 : 10000))) % 10;
    long long param = emu->mem[emu->ip + offset];
    long long addr = (mode == 2) ? emu->rel_base + param : param;
    ensure_mem(emu, addr);
    return addr;
}

static void emu_write_string(Emulator* emu, const char* s) {
    for (; *s; s++) {
        if ((emu->input_tail + 1) % emu->input_cap == emu->input_head) {
            int new_cap = emu->input_cap * 2;
            long long* new_q = malloc(new_cap * sizeof(long long));
            int i = 0;
            while (emu->input_head != emu->input_tail) {
                new_q[i++] = emu->input_q[emu->input_head];
                emu->input_head = (emu->input_head + 1) % emu->input_cap;
            }
            free(emu->input_q);
            emu->input_q = new_q;
            emu->input_head = 0;
            emu->input_tail = i;
            emu->input_cap = new_cap;
        }
        emu->input_q[emu->input_tail] = *s;
        emu->input_tail = (emu->input_tail + 1) % emu->input_cap;
    }
}

static int emulate(Emulator* emu, long long* out_val) {
    while (1) {
        ensure_mem(emu, emu->ip);
        long long opcode = emu->mem[emu->ip] % 100;
        switch (opcode) {
            case 1: emu->mem[get_write_addr(emu, 3)] = get_param_val(emu, 1) + get_param_val(emu, 2); emu->ip += 4; break;
            case 2: emu->mem[get_write_addr(emu, 3)] = get_param_val(emu, 1) * get_param_val(emu, 2); emu->ip += 4; break;
            case 3:
                if (emu->input_head == emu->input_tail) return WAITING;
                emu->mem[get_write_addr(emu, 1)] = emu->input_q[emu->input_head];
                emu->input_head = (emu->input_head + 1) % emu->input_cap;
                emu->ip += 2;
                break;
            case 4: *out_val = get_param_val(emu, 1); emu->ip += 2; return OUTPUT;
            case 5: emu->ip = (get_param_val(emu, 1) != 0) ? get_param_val(emu, 2) : emu->ip + 3; break;
            case 6: emu->ip = (get_param_val(emu, 1) == 0) ? get_param_val(emu, 2) : emu->ip + 3; break;
            case 7: emu->mem[get_write_addr(emu, 3)] = (get_param_val(emu, 1) < get_param_val(emu, 2)); emu->ip += 4; break;
            case 8: emu->mem[get_write_addr(emu, 3)] = (get_param_val(emu, 1) == get_param_val(emu, 2)); emu->ip += 4; break;
            case 9: emu->rel_base += get_param_val(emu, 1); emu->ip += 2; break;
            case 99: return HALTED;
            default: return HALTED;
        }
    }
}

typedef struct PathQueueNode {
    Room* room;
    Room** path;
    int path_len;
    struct PathQueueNode* next;
} PathQueueNode;

static Room** find_path(Room* from, Room* to) {
    PathQueueNode* q_head = malloc(sizeof(PathQueueNode));
    q_head->room = from;
    q_head->path = malloc(sizeof(Room*));
    q_head->path[0] = from;
    q_head->path_len = 1;
    q_head->next = NULL;
    PathQueueNode* q_tail = q_head;
    Map* visited = map_create();
    map_put(visited, from->name, (void*)1);

    while (q_head) {
        PathQueueNode* curr = q_head;
        q_head = q_head->next;
        if (!q_head) q_tail = NULL;

        if (curr->room == to) {
            Room** final_path = malloc((curr->path_len + 1) * sizeof(Room*));
            memcpy(final_path, curr->path, curr->path_len * sizeof(Room*));
            final_path[curr->path_len] = NULL;
            free(curr->path);
            free(curr);
            map_destroy(visited, NULL);
            while(q_head){
                PathQueueNode* temp = q_head;
                q_head = q_head->next;
                free(temp->path);
                free(temp);
            }
            return final_path;
        }

        Map* conns = curr->room->connections;
        for (int i = 0; i < conns->capacity; i++) {
            for (MapNode* node = conns->buckets[i]; node; node = node->next) {
                Room* neighbor = node->value;
                if (neighbor && !map_get(visited, neighbor->name)) {
                    map_put(visited, neighbor->name, (void*)1);
                    PathQueueNode* next_node = malloc(sizeof(PathQueueNode));
                    next_node->room = neighbor;
                    next_node->path_len = curr->path_len + 1;
                    next_node->path = malloc(next_node->path_len * sizeof(Room*));
                    memcpy(next_node->path, curr->path, curr->path_len * sizeof(Room*));
                    next_node->path[curr->path_len] = neighbor;
                    next_node->next = NULL;
                    if (q_tail) q_tail->next = next_node; else q_head = next_node;
                    q_tail = next_node;
                }
            }
        }
        free(curr->path);
        free(curr);
    }
    map_destroy(visited, NULL);
    return NULL;
}

static const char* get_opposite(const char* dir) {
    if (strcmp(dir, "north") == 0) return "south";
    if (strcmp(dir, "south") == 0) return "north";
    if (strcmp(dir, "east") == 0) return "west";
    if (strcmp(dir, "west") == 0) return "east";
    return "";
}

static bool is_blacklisted(const char* item) {
    const char* blacklist[] = {"photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet", NULL};
    for (int i = 0; blacklist[i]; i++) {
        if (strcmp(item, blacklist[i]) == 0) return true;
    }
    return false;
}

static void free_room(void* r) {
    Room* room = r;
    free(room->name);
    map_destroy(room->connections, NULL);
    free(room);
}

static char* rtrim(char* s) {
    char* end = s + strlen(s) - 1;
    while (end >= s && isspace((unsigned char)*end)) end--;
    *(end + 1) = '\0';
    return s;
}

int main() {
    FILE* f = fopen("input.txt", "r");
    long long* prog = malloc(INITIAL_MEM_CAP * sizeof(long long));
    int prog_size = 0;
    while (fscanf(f, "%lld,", &prog[prog_size]) == 1) prog_size++;
    fclose(f);

    Emulator emu = { .mem = prog, .mem_cap = INITIAL_MEM_CAP, .ip = 0, .rel_base = 0,
                     .input_q = malloc(INITIAL_INPUT_CAP * sizeof(long long)), .input_head = 0, .input_tail = 0, .input_cap = INITIAL_INPUT_CAP };
    if(prog_size > INITIAL_MEM_CAP) ensure_mem(&emu, prog_size);

    Map* world = map_create();
    Map* inventory = map_create();
    enum Mode mode = EXPLORE;
    Room** path = NULL; int path_len = 0;
    Room* checkpoint = NULL, *floor_room = NULL, *current_room = NULL, *last_room = NULL;
    char *test_dir = NULL, *last_dir = NULL;
    char** available_items = NULL; int available_items_count = 0;
    int item_mask = 0;
    char output_buf[MAX_OUTPUT_BUF]; int output_len = 0;

    while (true) {
        long long out_val;
        int status = emulate(&emu, &out_val);

        if (status == HALTED) {
            output_buf[output_len] = '\0';
            char* p = strstr(output_buf, "typing ");
            if (p) printf("%lld\n", atoll(p + 7));
            break;
        }
        if (status == OUTPUT) {
            if (output_len < MAX_OUTPUT_BUF - 1) output_buf[output_len++] = (char)out_val;
            continue;
        }

        output_buf[output_len] = '\0';
        output_len = 0;

        char* lines[MAX_LINES];
        int line_count = 0;
        char* line_ptr = strtok(output_buf, "\n");
        while(line_ptr && line_count < MAX_LINES) {
            lines[line_count++] = line_ptr;
            line_ptr = strtok(NULL, "\n");
        }

        Room* parsed_room = NULL;
        char* current_items[32]; int items_count = 0;
        char parse_buf[PARSE_BUF_SIZE];

        for (int i = 0; i < line_count; i++) {
            char* line = rtrim(lines[i]);
            if (strlen(line) == 0 || strcmp(line, "Command?") == 0) continue;

            char* p;
            if ((p = strstr(line, "== "))) {
                char* p_end = strstr(p + 3, " ==");
                if(p_end) {
                    strncpy(parse_buf, p + 3, p_end - (p + 3));
                    parse_buf[p_end - (p + 3)] = '\0';
                    parsed_room = map_get(world, parse_buf);
                    if (!parsed_room) {
                        parsed_room = malloc(sizeof(Room));
                        parsed_room->name = strdup(parse_buf);
                        parsed_room->connections = map_create();
                        map_put(world, parsed_room->name, parsed_room);
                    }
                    current_room = parsed_room;
                }
            } else if (strcmp(line, "Doors here lead:") == 0) {
                while (++i < line_count && lines[i][0] == '-') {
                    if (current_room && !map_get(current_room->connections, rtrim(lines[i] + 2))) 
                        map_put(current_room->connections, rtrim(lines[i] + 2), NULL);
                }
                i--;
            } else if (strcmp(line, "Items here:") == 0) {
                while (++i < line_count && lines[i][0] == '-') {
                    if (items_count < 32) current_items[items_count++] = rtrim(lines[i] + 2);
                }
                i--;
            } else if ((p = strstr(line, "You take the "))) {
                strncpy(parse_buf, p + 13, strlen(p + 13) - 1); parse_buf[strlen(p+13)-1] = '\0';
                map_put(inventory, parse_buf, (void*)1);
            } else if ((p = strstr(line, "You drop the "))) {
                strncpy(parse_buf, p + 13, strlen(p + 13) - 1); parse_buf[strlen(p+13)-1] = '\0';
                map_put(inventory, parse_buf, NULL);
            } else if (strstr(line, "Alert!")) {
                if (mode == EXPLORE) {
                    if (path_len > 0) path_len--;
                    checkpoint = last_room;
                    floor_room = current_room;
                    if(test_dir) free(test_dir);
                    test_dir = strdup(last_dir);
                    if (checkpoint && test_dir) map_put(checkpoint->connections, test_dir, floor_room);
                }
                last_room = NULL;
                free(last_dir); last_dir = NULL;
            }
        }

        if (last_room && last_dir && current_room && !map_get(last_room->connections, last_dir)) {
            map_put(last_room->connections, last_dir, current_room);
            map_put(current_room->connections, get_opposite(last_dir), last_room);
        }
        last_room = current_room;
        if(last_dir) free(last_dir);
        last_dir = NULL;

        char cmd_buf[64];
        if (mode == EXPLORE) {
            bool took_item = false;
            for (int i = 0; i < items_count; i++) {
                if (!is_blacklisted(current_items[i])) {
                    snprintf(cmd_buf, sizeof(cmd_buf), "take %s\n", current_items[i]);
                    emu_write_string(&emu, cmd_buf);
                    took_item = true;
                    break;
                }
            }
            if (!took_item) {
                char* target_dir = NULL;
                Map* conns = current_room->connections;
                 for (int i = 0; i < conns->capacity; i++) {
                    for (MapNode* node = conns->buckets[i]; node; node = node->next) {
                        if (!node->value) { target_dir = node->key; break; }
                    }
                    if (target_dir) break;
                }
                if (target_dir) {
                    path = realloc(path, (path_len + 1) * sizeof(Room*));
                    path[path_len++] = current_room;
                    last_dir = strdup(target_dir);
                    snprintf(cmd_buf, sizeof(cmd_buf), "%s\n", target_dir);
                    emu_write_string(&emu, cmd_buf);
                } else if (path_len > 0) {
                    Room* prev_room = path[--path_len];
                    char* back_dir = NULL;
                    Map* conns = current_room->connections;
                    for (int i = 0; i < conns->capacity; i++) {
                        for (MapNode* node = conns->buckets[i]; node; node = node->next) {
                            if (node->value == prev_room) { back_dir = node->key; break; }
                        }
                        if (back_dir) break;
                    }
                    if (back_dir) {
                        last_dir = strdup(back_dir);
                        snprintf(cmd_buf, sizeof(cmd_buf), "%s\n", back_dir);
                        emu_write_string(&emu, cmd_buf);
                    }
                } else if (checkpoint) {
                    if (path) free(path);
                    path = find_path(current_room, checkpoint);
                    path_len = 0;
                    if(path) for(; path[path_len]; path_len++);
                    mode = NAVIGATE;
                }
            }
        }
        if (mode == NAVIGATE) {
            if (path && path_len > 1) {
                Room* next_room = path[1];
                char* dir = NULL;
                Map* conns = current_room->connections;
                for (int i = 0; i < conns->capacity; i++) {
                    for (MapNode* node = conns->buckets[i]; node; node = node->next) {
                        if (node->value == next_room) { dir = node->key; break; }
                    }
                    if (dir) break;
                }
                if (dir) {
                    last_dir = strdup(dir);
                    snprintf(cmd_buf, sizeof(cmd_buf), "%s\n", dir);
                    emu_write_string(&emu, cmd_buf);
                    memmove(path, path + 1, (path_len - 1) * sizeof(Room*));
                    path_len--;
                }
            } else {
                Map* inv = inventory;
                for(int i=0; i<inv->capacity; i++){
                    for(MapNode* n = inv->buckets[i]; n; n=n->next){
                        if(n->value){
                             available_items = realloc(available_items, (available_items_count + 1) * sizeof(char*));
                             available_items[available_items_count++] = strdup(n->key);
                        }
                    }
                }
                mode = TEST;
            }
        }
        if (mode == TEST) {
            bool processed = false;
            for (int i = 0; i < available_items_count; i++) {
                bool should_have = (item_mask >> i) & 1;
                bool has = map_get(inventory, available_items[i]) != NULL;
                if (should_have != has) {
                    snprintf(cmd_buf, sizeof(cmd_buf), "%s %s\n", should_have ? "take" : "drop", available_items[i]);
                    emu_write_string(&emu, cmd_buf);
                    processed = true;
                    break;
                }
            }
            if (!processed) {
                item_mask++;
                if (test_dir) {
                    snprintf(cmd_buf, sizeof(cmd_buf), "%s\n", test_dir);
                    emu_write_string(&emu, cmd_buf);
                }
            }
        }
    }

    free(emu.mem);
    free(emu.input_q);
    map_destroy(world, free_room);
    map_destroy(inventory, NULL);
    if(path) free(path);
    if(test_dir) free(test_dir);
    if(last_dir) free(last_dir);
    for(int i=0; i < available_items_count; i++) free(available_items[i]);
    if(available_items) free(available_items);

    return 0;
}
