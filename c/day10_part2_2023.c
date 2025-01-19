
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int x;
    int y;
} Coord;

Coord coord_add(Coord c1, Coord c2) {
    return (Coord){c1.x + c2.x, c1.y + c2.y};
}

Coord coord_subtract(Coord c1, Coord c2) {
    return (Coord){c1.x - c2.x, c1.y - c2.y};
}

Coord coord_opposite(Coord c) {
    return (Coord){-c.x, -c.y};
}

typedef char Tile;
typedef struct {
    bool top;
    bool right;
    bool bottom;
    bool left;
} Pipe;

typedef struct {
    int width;
    int height;
    Tile* data;
} Grid;

const Coord UNDEFINED = {0, 0};
const Coord TOP = {0, -1};
const Coord RIGHT = {1, 0};
const Coord BOTTOM = {0, 1};
const Coord LEFT = {-1, 0};

const Tile EMPTY = '.';
const Tile START = 'S';
const Tile VERTICAL = '|';
const Tile HORIZONTAL = '-';
const Tile TOP_LEFT_CORNER = 'J';
const Tile TOP_RIGHT_CORNER = 'L';
const Tile BOTTOM_LEFT_CORNER = '7';
const Tile BOTTOM_RIGHT_CORNER = 'F';
const Tile ENCLOSED = 'X';

const Pipe VERTICAL_PIPE = {true, false, true, false};
const Pipe HORIZONTAL_PIPE = {false, true, false, true};
const Pipe TOP_LEFT_CORNER_PIPE = {true, false, false, true};
const Pipe TOP_RIGHT_CORNER_PIPE = {true, true, false, false};
const Pipe BOTTOM_LEFT_CORNER_PIPE = {false, false, true, true};
const Pipe BOTTOM_RIGHT_CORNER_PIPE = {false, true, true, false};


Pipe get_pipe_from_tile(Tile tile) {
    switch (tile) {
        case VERTICAL: return VERTICAL_PIPE;
        case HORIZONTAL: return HORIZONTAL_PIPE;
        case TOP_LEFT_CORNER: return TOP_LEFT_CORNER_PIPE;
        case TOP_RIGHT_CORNER: return TOP_RIGHT_CORNER_PIPE;
        case BOTTOM_LEFT_CORNER: return BOTTOM_LEFT_CORNER_PIPE;
        case BOTTOM_RIGHT_CORNER: return BOTTOM_RIGHT_CORNER_PIPE;
        default: return (Pipe){false, false, false, false};
    }
}

Tile get_tile_from_pipe(Pipe pipe) {
    if (memcmp(&pipe, &VERTICAL_PIPE, sizeof(Pipe)) == 0) return VERTICAL;
    if (memcmp(&pipe, &HORIZONTAL_PIPE, sizeof(Pipe)) == 0) return HORIZONTAL;
    if (memcmp(&pipe, &TOP_LEFT_CORNER_PIPE, sizeof(Pipe)) == 0) return TOP_LEFT_CORNER;
    if (memcmp(&pipe, &TOP_RIGHT_CORNER_PIPE, sizeof(Pipe)) == 0) return TOP_RIGHT_CORNER;
    if (memcmp(&pipe, &BOTTOM_LEFT_CORNER_PIPE, sizeof(Pipe)) == 0) return BOTTOM_LEFT_CORNER;
    if (memcmp(&pipe, &BOTTOM_RIGHT_CORNER_PIPE, sizeof(Pipe)) == 0) return BOTTOM_RIGHT_CORNER;
    return EMPTY;
}

bool pipe_is_equal(Pipe pipe1, Pipe pipe2) {
    return memcmp(&pipe1, &pipe2, sizeof(Pipe)) == 0;
}

Grid build_grid(char** input, int height, int width) {
    Grid grid;
    grid.width = width;
    grid.height = height;
    grid.data = (Tile*)malloc(sizeof(Tile) * width * height);
    memset(grid.data, EMPTY, sizeof(Tile) * width * height);

    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
           if(input[y][x] != EMPTY)
            grid.data[y * width + x] = input[y][x];
        }
    }
    return grid;
}


void free_grid(Grid grid) {
    free(grid.data);
}


Coord find_start(Grid grid) {
    for (int y = 0; y < grid.height; y++) {
        for (int x = 0; x < grid.width; x++) {
            if (grid.data[y * grid.width + x] == START) {
                return (Coord){x, y};
            }
        }
    }
    return UNDEFINED;
}

Pipe get_pipe_from_neighbors(Coord c, Grid grid) {
    Pipe pipe = {false, false, false, false};
    Coord possible_neighbors[4] = {
            coord_add(c, TOP),
            coord_add(c, RIGHT),
            coord_add(c, BOTTOM),
            coord_add(c, LEFT)
    };
    Coord dirs[4] = {TOP, RIGHT, BOTTOM, LEFT};


    for (int i = 0; i < 4; i++) {
      Coord neighbor_coord = possible_neighbors[i];

      if (neighbor_coord.x >= 0 && neighbor_coord.x < grid.width && neighbor_coord.y >= 0 && neighbor_coord.y < grid.height) {
        Pipe neighbor_pipe = get_pipe_from_tile(grid.data[neighbor_coord.y * grid.width + neighbor_coord.x]);
        Coord opp = coord_opposite(dirs[i]);
        if ((opp.x == 0 && opp.y == -1 && neighbor_pipe.top) ||
            (opp.x == 1 && opp.y == 0 && neighbor_pipe.right) ||
            (opp.x == 0 && opp.y == 1 && neighbor_pipe.bottom) ||
            (opp.x == -1 && opp.y == 0 && neighbor_pipe.left))
        {
           if(dirs[i].x == 0 && dirs[i].y == -1) pipe.top = true;
           if(dirs[i].x == 1 && dirs[i].y == 0) pipe.right = true;
           if(dirs[i].x == 0 && dirs[i].y == 1) pipe.bottom = true;
           if(dirs[i].x == -1 && dirs[i].y == 0) pipe.left = true;

        }
      }

    }
    return pipe;
}


Coord* path_finding(Coord start, Grid grid, int *path_len) {
    Coord* path = (Coord*)malloc(sizeof(Coord) * grid.width * grid.height);
    int path_index = 0;
    path[path_index++] = start;

    Pipe start_pipe = get_pipe_from_neighbors(start, grid);
    Coord previous_dir;
    Coord current;

     if (start_pipe.top) {
        previous_dir = TOP;
        current = coord_add(start, TOP);
    } else if (start_pipe.right) {
        previous_dir = RIGHT;
        current = coord_add(start, RIGHT);
    } else if (start_pipe.bottom) {
        previous_dir = BOTTOM;
        current = coord_add(start, BOTTOM);
    } else {
        previous_dir = LEFT;
        current = coord_add(start, LEFT);
    }


    while (current.x != start.x || current.y != start.y) {
        path[path_index++] = current;

        Pipe current_pipe = get_pipe_from_tile(grid.data[current.y * grid.width + current.x]);
         if (current_pipe.top && (previous_dir.x != 0 || previous_dir.y != 1)){
           previous_dir = TOP;
            current = coord_add(current, TOP);
        } else if (current_pipe.right && (previous_dir.x != -1 || previous_dir.y != 0)) {
           previous_dir = RIGHT;
            current = coord_add(current, RIGHT);
        } else if (current_pipe.bottom && (previous_dir.x != 0 || previous_dir.y != -1)) {
            previous_dir = BOTTOM;
            current = coord_add(current, BOTTOM);
        }else{
            previous_dir = LEFT;
            current = coord_add(current, LEFT);
        }
    }
    *path_len = path_index;
    return path;
}


Grid get_path_grid(Grid grid, Coord* path, int path_len, Tile empty) {
    Grid new_grid;
    new_grid.width = grid.width;
    new_grid.height = grid.height;
    new_grid.data = (Tile*)malloc(sizeof(Tile) * grid.width * grid.height);
      memset(new_grid.data, EMPTY, sizeof(Tile) * grid.width * grid.height);


    for (int i = 0; i < path_len; i++) {
      new_grid.data[path[i].y * grid.width + path[i].x] = grid.data[path[i].y * grid.width + path[i].x];
    }

    Coord start = path[0];
    new_grid.data[start.y * grid.width + start.x] = get_tile_from_pipe(get_pipe_from_neighbors(start, grid));

    return new_grid;
}

bool is_inside(Coord c, Grid grid, Tile empty) {
    if (grid.data[c.y * grid.width + c.x] != EMPTY) {
        return false;
    }

    Tile start_pipe = empty;
    int num_pipe_on_left = 0;

    for (int x = 0; x < c.x; x++) {
        Coord coord = {x, c.y};
        Tile v = grid.data[coord.y * grid.width + coord.x];

        switch (v) {
            case VERTICAL:
                num_pipe_on_left++;
                break;
            case TOP_RIGHT_CORNER:
                start_pipe = TOP_RIGHT_CORNER;
                break;
            case BOTTOM_RIGHT_CORNER:
                start_pipe = BOTTOM_RIGHT_CORNER;
                break;
            case TOP_LEFT_CORNER:
                if (start_pipe == BOTTOM_RIGHT_CORNER) {
                    start_pipe = empty;
                    num_pipe_on_left++;
                } else if (v == TOP_RIGHT_CORNER) {
                    start_pipe = empty;
                }
                break;
            case BOTTOM_LEFT_CORNER:
                if (start_pipe == TOP_RIGHT_CORNER) {
                    start_pipe = empty;
                    num_pipe_on_left++;
                } else if (start_pipe == BOTTOM_RIGHT_CORNER) {
                    start_pipe = empty;
                }
                break;
        }
    }
    return num_pipe_on_left % 2 == 1;
}

int solve(char** input, int height, int width) {
    Grid grid = build_grid(input, height, width);
    Coord start = find_start(grid);

    int path_len;
    Coord* path = path_finding(start, grid, &path_len);
    Grid path_grid = get_path_grid(grid, path, path_len, EMPTY);
    free_grid(grid);
    free(path);
    
    int cnt = 0;
    for (int y = 0; y < path_grid.height; y++) {
        for (int x = 0; x < path_grid.width; x++) {
            Coord c = {x, y};
            if (is_inside(c, path_grid, EMPTY)) {
                cnt++;
            }
        }
    }
    free_grid(path_grid);
    return cnt;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char** lines = NULL;
    size_t line_len = 0;
    ssize_t read;
    char* line = NULL;
    int line_count = 0;
    int width = 0;

    while ((read = getline(&line, &line_len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
            read--;
        }
        
        if(line_count == 0){
             width = read;
        }

        lines = (char**)realloc(lines, sizeof(char*) * (line_count + 1));
        lines[line_count] = strdup(line);
        line_count++;
    }

    fclose(file);
    if(line) free(line);
    
    int result = solve(lines, line_count, width);
    printf("%d\n", result);

    for (int i = 0; i < line_count; i++) {
        free(lines[i]);
    }
    free(lines);

    return 0;
}
