
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_Y 200
#define MAX_X 200
#define MAX_STEPS 20000
#define MAX_BUFFER_SIZE (MAX_Y * MAX_X + MAX_STEPS + 100) // Rough estimate for input buffer

typedef struct {
    int x;
    int y;
} Point;

char initial_map[MAX_Y][MAX_X];
int height = 0;
int width = 0;
Point initial_robot;

Point steps[MAX_STEPS];
int num_steps = 0;

bool try_to_step(char current_map[MAX_Y][MAX_X], Point pos, Point dir);
long long calculate_score(char current_map[MAX_Y][MAX_X]);
void parse_input_from_string(const char* buffer);
void scale_up(const char* input_str, char* output_str, long input_size);


long long solve(const char* input_buffer) {
    height = 0;
    width = 0;
    num_steps = 0;
    memset(initial_map, ' ', sizeof(initial_map));

    parse_input_from_string(input_buffer);

    char working_map[MAX_Y][MAX_X];
    memcpy(working_map, initial_map, sizeof(initial_map));
    Point current_robot = initial_robot;

    for (int i = 0; i < num_steps; ++i) {
        Point dir = steps[i];
        Point next_robot_pos = {current_robot.x + dir.x, current_robot.y + dir.y};

        if (next_robot_pos.x < 0 || next_robot_pos.x >= width || next_robot_pos.y < 0 || next_robot_pos.y >= height) {
            continue;
        }
        char target_char = working_map[next_robot_pos.y][next_robot_pos.x];
        if (target_char == '#') {
            continue;
        }

        if (try_to_step(working_map, next_robot_pos, dir)) {
            working_map[next_robot_pos.y][next_robot_pos.x] = '@';
            working_map[current_robot.y][current_robot.x] = '.';
            current_robot = next_robot_pos;
        }
    }

    return calculate_score(working_map);
}

bool try_to_step(char current_map[MAX_Y][MAX_X], Point pos, Point dir) {
    if (pos.x < 0 || pos.x >= width || pos.y < 0 || pos.y >= height) {
        return false;
    }

    char original_map[MAX_Y][MAX_X];
    memcpy(original_map, current_map, sizeof(original_map));

    char thing_to_move = current_map[pos.y][pos.x];
    Point destination_pos = {pos.x + dir.x, pos.y + dir.y};

    if (destination_pos.x < 0 || destination_pos.x >= width || destination_pos.y < 0 || destination_pos.y >= height) {
         return false;
    }

    bool success = false;

    if (thing_to_move == '.' ) {
        success = true;
    } else if (thing_to_move == '#') {
        success = false;
    } else if (thing_to_move == 'O' || thing_to_move == '@') {
        if (try_to_step(current_map, destination_pos, dir)) {
            current_map[destination_pos.y][destination_pos.x] = thing_to_move;
            current_map[pos.y][pos.x] = '.';
            success = true;
        }
    } else if (thing_to_move == '[') {
        Point right_part_pos = {pos.x + 1, pos.y};
         if (right_part_pos.x < width && current_map[right_part_pos.y][right_part_pos.x] == ']') {
              if (dir.x == -1 && dir.y == 0) {
                  Point left_dest = {pos.x - 1, pos.y};
                  if (try_to_step(current_map, left_dest, dir)) {
                      current_map[left_dest.y][left_dest.x] = '[';
                      current_map[pos.y][pos.x] = ']';
                      current_map[right_part_pos.y][right_part_pos.x] = '.';
                      success = true;
                  }
              } else if (dir.x == 1 && dir.y == 0) {
                  Point far_right_dest = {right_part_pos.x + 1, right_part_pos.y};
                  if (try_to_step(current_map, far_right_dest, dir)) {
                       current_map[far_right_dest.y][far_right_dest.x] = ']';
                       current_map[right_part_pos.y][right_part_pos.x] = '[';
                       current_map[pos.y][pos.x] = '.';
                      success = true;
                  }
              } else {
                  Point left_dest = {pos.x + dir.x, pos.y + dir.y};
                  Point right_dest = {right_part_pos.x + dir.x, right_part_pos.y + dir.y};
                   if (try_to_step(current_map, left_dest, dir) && try_to_step(current_map, right_dest, dir)) {
                      current_map[left_dest.y][left_dest.x] = '[';
                      current_map[right_dest.y][right_dest.x] = ']';
                      current_map[pos.y][pos.x] = '.';
                      current_map[right_part_pos.y][right_part_pos.x] = '.';
                      success = true;
                  }
              }
         } else { success = false; }
    } else if (thing_to_move == ']') {
        // Check if the left part is '['
        Point left_part_pos = {pos.x - 1, pos.y};
        if (left_part_pos.x >= 0 && current_map[left_part_pos.y][left_part_pos.x] == '[') {
            // Try pushing the entire '[]' unit, starting from the left part '['
            if (try_to_step(current_map, left_part_pos, dir)){
                 success = true; // The push logic is handled when try_to_step processes '['
            } else {
                 success = false;
            }
        } else {
             success = false; // Standalone ']' cannot be pushed
        }
    }

    if (!success) {
        memcpy(current_map, original_map, sizeof(original_map));
    }
    return success;
}


void scale_up(const char* input_str, char* output_str, long input_size) {
    char* out_ptr = output_str;
    const char* in_ptr = input_str;
    const char* end_ptr = input_str + input_size;
    bool map_section = true;

    while (in_ptr < end_ptr) {
        char c = *in_ptr++;
        if (c == '\n' && in_ptr < end_ptr && *in_ptr == '\n') {
             *out_ptr++ = '\n';
             *out_ptr++ = '\n';
             in_ptr++;
             map_section = false;
             continue;
        }

        if (map_section) {
            switch (c) {
                case '#': *out_ptr++ = '#'; *out_ptr++ = '#'; break;
                case '.': *out_ptr++ = '.'; *out_ptr++ = '.'; break;
                case 'O': *out_ptr++ = '['; *out_ptr++ = ']'; break;
                case '@': *out_ptr++ = '@'; *out_ptr++ = '.'; break;
                case '\n': *out_ptr++ = '\n'; break;
                default: *out_ptr++ = c; *out_ptr++ = c; break; // Should not happen? Assume space maybe?
            }
        } else {
             *out_ptr++ = c; // Copy move characters and newlines as is
        }
    }
    *out_ptr = '\0';
}

 void parse_input_from_string(const char* buffer) {
    int y = 0;
    int x = 0;
    bool parsing_map = true;
    const char* ptr = buffer;
    int max_x = 0;

    while (*ptr && y < MAX_Y) {
        char c = *ptr;
        if (parsing_map) {
            if (c == '\n') {
                if (ptr > buffer && *(ptr - 1) == '\n') {
                    parsing_map = false;
                    ptr++;
                    continue;
                }
                 if (x > max_x) max_x = x;
                 y++;
                 x = 0;
                 if (y >= MAX_Y) break;
            } else {
                if (x < MAX_X) {
                    initial_map[y][x] = c;
                    if (c == '@') {
                        initial_robot.x = x;
                        initial_robot.y = y;
                    }
                    x++;
                }
            }
        } else {
            if (c != '\n' && num_steps < MAX_STEPS) {
               if(c == '^') steps[num_steps++] = (Point){0, -1};
               else if(c == 'v') steps[num_steps++] = (Point){0, 1};
               else if(c == '<') steps[num_steps++] = (Point){-1, 0};
               else if(c == '>') steps[num_steps++] = (Point){1, 0};
            }
        }
        ptr++;
    }

    if (parsing_map && x > 0) { // Capture last line of map if no trailing newline
         if (x > max_x) max_x = x;
         y++;
    }

    height = y;
    width = max_x;

    // Ensure map is rectangular based on max_x, filling gaps with space
     for(int r=0; r < height; ++r) {
         for (int c = 0; c < width; ++c) {
            if (initial_map[r][c] == 0) { // Check if character was written
                 initial_map[r][c] = ' '; // Fill unspecified parts with space
            }
         }
     }
}

long long calculate_score(char current_map[MAX_Y][MAX_X]) {
    long long score = 0;
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            if (current_map[y][x] == '[' || current_map[y][x] == 'O') {
                score += (long long)x + 100LL * y;
            }
        }
    }
    return score;
}


int main() {
    FILE *f = fopen("input.txt", "rb");
    if (!f) { fprintf(stderr, "Error opening input.txt\n"); return 1; }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *original_input = malloc(fsize + 1);
    if (!original_input) { fclose(f); fprintf(stderr, "Malloc failed\n"); return 1; }
    fread(original_input, 1, fsize, f);
    fclose(f);
    original_input[fsize] = 0;

    long long result1 = solve(original_input);
    printf("%lld\n", result1);

    long scaled_size_needed = 0;
    const char* p = original_input;
    const char* end = original_input + fsize;
    bool map_sec = true;
     while(p < end){
        char c = *p++;
         if (c == '\n' && p < end && *p == '\n'){
             scaled_size_needed += 2;
             p++;
             map_sec = false;
             continue;
         }
         if(map_sec){
             if (c == '#' || c == '.' || c == 'O' || c == '@') scaled_size_needed += 2;
             else scaled_size_needed += 1;
         } else {
            scaled_size_needed += 1;
         }
     }


    char *scaled_input = malloc(scaled_size_needed + 1);
     if (!scaled_input) { free(original_input); fprintf(stderr, "Malloc failed\n"); return 1; }
    scale_up(original_input, scaled_input, fsize);

    long long result2 = solve(scaled_input);
    printf("%lld\n", result2);

    free(original_input);
    free(scaled_input);

    return 0;
}

