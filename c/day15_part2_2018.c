
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#define MAX_Y 50
#define MAX_X 50
#define MAX_UNITS 50
#define MAX_QUEUE (MAX_Y * MAX_X)

typedef enum {
    KIND_SPACE = 1,
    KIND_ELF = 2,
    KIND_GOBLIN = 4,
    KIND_WALL = 8
} Kind;

typedef struct {
    int x, y;
} Point;

typedef struct {
    Kind kind;
    int x, y;
} Tile;

typedef struct Unit {
    int id;
    Kind kind;
    int hp;
    int power;
    int x, y;
    bool alive;
} Unit;

Tile map[MAX_Y][MAX_X];
Unit units[MAX_UNITS];
Unit initial_units[MAX_UNITS];
int rows = 0, cols = 0;
int num_units = 0;
int initial_num_units = 0;
Tile initial_map[MAX_Y][MAX_X];
int initial_rows = 0, initial_cols = 0;


Point offsets[4] = {{0, -1}, {-1, 0}, {1, 0}, {0, 1}};

Point queue[MAX_QUEUE];
int q_head, q_tail;
int dist[MAX_Y][MAX_X];
Point path[MAX_Y][MAX_X];

void enqueue(Point p) {
    queue[q_tail++] = p;
}

Point dequeue() {
    return queue[q_head++];
}

bool is_queue_empty() {
    return q_head == q_tail;
}

void reset_queue() {
    q_head = q_tail = 0;
}

int unit_compare(const void *a, const void *b) {
    Unit *ua = (Unit *)a;
    Unit *ub = (Unit *)b;
    if (ua->y != ub->y) return ua->y - ub->y;
    return ua->x - ub->x;
}

int point_compare(const void *a, const void *b) {
    Point *pa = (Point *)a;
    Point *pb = (Point *)b;
     if (pa->y != pb->y) return pa->y - pb->y;
    return pa->x - pb->x;
}

void bfs(Point start) {
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            dist[y][x] = -1;
            path[y][x] = (Point){-1, -1};
        }
    }

    reset_queue();
    dist[start.y][start.x] = 0;
    enqueue(start);

    while (!is_queue_empty()) {
        Point curr = dequeue();

        for (int i = 0; i < 4; i++) {
            Point next = {curr.x + offsets[i].x, curr.y + offsets[i].y};

            if (next.x >= 0 && next.x < cols && next.y >= 0 && next.y < rows &&
                map[next.y][next.x].kind == KIND_SPACE && dist[next.y][next.x] == -1)
            {
                dist[next.y][next.x] = dist[curr.y][curr.x] + 1;
                path[next.y][next.x] = curr;
                enqueue(next);
            }
        }
    }
}

void parse_map(FILE *fp, int elf_power) {
    char line[MAX_X + 2];
    rows = 0;
    num_units = 0;
    int unit_id_counter = 0;
    while (fgets(line, sizeof(line), fp) != NULL) {
        cols = strlen(line) - 1;
        if (line[cols] == '\n') line[cols] = '\0'; else cols++; // Handle no newline at EOF

        for (int x = 0; x < cols; x++) {
            Kind kind;
            switch (line[x]) {
                case '#': kind = KIND_WALL; break;
                case '.': kind = KIND_SPACE; break;
                case 'E': kind = KIND_ELF; break;
                case 'G': kind = KIND_GOBLIN; break;
                default: kind = KIND_SPACE; // Should not happen
            }
            map[rows][x] = (Tile){kind, x, rows};
            if (kind == KIND_ELF || kind == KIND_GOBLIN) {
                units[num_units++] = (Unit){
                    unit_id_counter++,
                    kind,
                    200,
                    (kind == KIND_ELF) ? elf_power : 3,
                    x,
                    rows,
                    true
                };
            }
        }
        rows++;
    }
    // Store initial state
    memcpy(initial_map, map, sizeof(map));
    memcpy(initial_units, units, sizeof(units));
    initial_rows = rows;
    initial_cols = cols;
    initial_num_units = num_units;
}

void reset_state() {
    memcpy(map, initial_map, sizeof(map));
    memcpy(units, initial_units, sizeof(units));
    rows = initial_rows;
    cols = initial_cols;
    num_units = initial_num_units;
     for(int i = 0; i < num_units; i++) {
        units[i].alive = true; // Ensure all are marked alive initially
        units[i].hp = 200; // Reset HP
    }
}

bool get_status(int *total_hp) {
    bool elves_exist = false;
    bool goblins_exist = false;
    *total_hp = 0;
    for (int i = 0; i < num_units; i++) {
        if (units[i].alive) {
            if (units[i].kind == KIND_ELF) elves_exist = true;
            else goblins_exist = true;
            *total_hp += units[i].hp;
        }
    }
    return elves_exist && goblins_exist;
}

bool tick(bool stop_on_elf_death, bool *elf_died_out) {
    qsort(units, num_units, sizeof(Unit), unit_compare);

    *elf_died_out = false;

    for (int i = 0; i < num_units; i++) {
        if (!units[i].alive) continue;

        Unit *current_unit = &units[i];
        Kind target_kind = (current_unit->kind == KIND_ELF) ? KIND_GOBLIN : KIND_ELF;
        bool targets_exist = false;
        for(int j = 0; j < num_units; j++){
            if(units[j].alive && units[j].kind == target_kind){
                targets_exist = true;
                break;
            }
        }
        if(!targets_exist) return false; // Combat ends

        // Check for adjacent targets
        Unit *attack_target = NULL;
        int min_hp = INT_MAX;

        for (int k = 0; k < 4; k++) {
            Point neighbor = {current_unit->x + offsets[k].x, current_unit->y + offsets[k].y};
            if (neighbor.x >= 0 && neighbor.x < cols && neighbor.y >= 0 && neighbor.y < rows) {
                 for(int j=0; j<num_units; ++j) {
                     if(units[j].alive && units[j].kind == target_kind && units[j].x == neighbor.x && units[j].y == neighbor.y) {
                          if (units[j].hp < min_hp) {
                            min_hp = units[j].hp;
                            attack_target = &units[j];
                        }
                        // Tie-break handled by reading order implicitly due to offset order
                        break; // Found unit at this neighbor location
                     }
                 }
            }
        }


        // Move if no adjacent target
        if (attack_target == NULL) {
            bfs((Point){current_unit->x, current_unit->y});

            Point targets[MAX_Y * MAX_X];
            int target_count = 0;
            int min_dist = INT_MAX;

            for (int j = 0; j < num_units; j++) {
                if (units[j].alive && units[j].kind == target_kind) {
                    for (int k = 0; k < 4; k++) {
                        Point neighbor = {units[j].x + offsets[k].x, units[j].y + offsets[k].y};
                         if (neighbor.x >= 0 && neighbor.x < cols && neighbor.y >= 0 && neighbor.y < rows &&
                             map[neighbor.y][neighbor.x].kind == KIND_SPACE)
                         {
                             if(dist[neighbor.y][neighbor.x] != -1) {
                                 if (dist[neighbor.y][neighbor.x] < min_dist) {
                                     min_dist = dist[neighbor.y][neighbor.x];
                                     target_count = 0;
                                     targets[target_count++] = neighbor;
                                 } else if (dist[neighbor.y][neighbor.x] == min_dist) {
                                     targets[target_count++] = neighbor;
                                 }
                             }
                         }
                    }
                }
            }


            if (target_count > 0) {
                qsort(targets, target_count, sizeof(Point), point_compare);
                Point chosen_target = targets[0];
                Point current_step = chosen_target;

                // Trace back path to find the first step
                while (dist[current_step.y][current_step.x] > 1) {
                    current_step = path[current_step.y][current_step.x];
                     if(current_step.x == -1) break; // Should not happen if dist > 1
                }


                if(current_step.x != -1 && dist[current_step.y][current_step.x] == 1) {
                    // Perform move
                    map[current_unit->y][current_unit->x].kind = KIND_SPACE;
                    current_unit->x = current_step.x;
                    current_unit->y = current_step.y;
                    map[current_unit->y][current_unit->x].kind = current_unit->kind;
                }


                // Re-check for adjacent targets after moving
                min_hp = INT_MAX; // Reset min_hp for attack check
                attack_target = NULL; // Reset attack_target
                 for (int k = 0; k < 4; k++) {
                    Point neighbor = {current_unit->x + offsets[k].x, current_unit->y + offsets[k].y};
                    if (neighbor.x >= 0 && neighbor.x < cols && neighbor.y >= 0 && neighbor.y < rows) {
                         for(int j=0; j<num_units; ++j) {
                            if(units[j].alive && units[j].kind == target_kind && units[j].x == neighbor.x && units[j].y == neighbor.y) {
                                if (units[j].hp < min_hp) {
                                    min_hp = units[j].hp;
                                    attack_target = &units[j];
                                }
                                break;
                            }
                         }
                    }
                 }
            }
        }


        // Attack if possible
        if (attack_target != NULL) {
            attack_target->hp -= current_unit->power;
            if (attack_target->hp <= 0) {
                attack_target->alive = false;
                map[attack_target->y][attack_target->x].kind = KIND_SPACE;
                if (attack_target->kind == KIND_ELF && stop_on_elf_death) {
                    *elf_died_out = true;
                    return true; // Return immediately if elf died and we care
                }
            }
        }
    }

    // Check if the round completed fully (i.e. the last unit acted)
    // The logic inside the loop handles early exit if combat ends.
    // If we reach here, the round was processed. Need to filter dead units for next round.
    int live_units = 0;
    for(int k=0; k<num_units; ++k){
        if(units[k].alive){
            units[live_units++] = units[k];
        }
    }
    num_units = live_units;


    return true; // Round completed or ended naturally
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    int elf_power = 3; // Start slightly below default to ensure the loop runs at least once for power 4
    int final_outcome = 0;

    while (true) {
        elf_power++;
        fseek(fp, 0, SEEK_SET); // Reset file pointer
        parse_map(fp, elf_power); // Parse with current elf power

        int rounds = 0;
        bool elf_died = false;

        while (true) {
            int hp_sum;
            bool combat_continues = get_status(&hp_sum);
            if (!combat_continues) {
                final_outcome = rounds * hp_sum;
                break;
            }

            bool round_completed = tick(true, &elf_died); // Pass true to stop on elf death


            if (elf_died) {
                 //printf("Elf died with power %d\n", elf_power);
                 break; // Break inner loop, try higher power
            }

            if (!round_completed && !elf_died){ // Combat ended mid-round without elf death
                 hp_sum = 0; // Recalculate final HP sum
                 get_status(&hp_sum);
                 final_outcome = rounds * hp_sum; // Use current round count
                 break; // Break inner loop, combat finished
            }


            rounds++;

        } // End simulation loop for this power level

        if (!elf_died) {
             //printf("Success with power %d\n", elf_power);
            break; // Found the minimum power without elf deaths
        }

        // Reset state for the next power level trial - parsing does this implicitly now
        // reset_state(); // Needed if not re-parsing

    } // End power level loop

    fclose(fp);
    printf("%d\n", final_outcome);

    return 0;
}

