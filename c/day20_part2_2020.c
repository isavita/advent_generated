
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define MAX_TILES 144 // 12*12
#define MAX_TILE_SIZE 10
#define MAX_LINE_LEN 128
#define MAX_IMAGE_DIM (MAX_TILES_SQRT * (MAX_TILE_SIZE - 2))
#define MAX_MONSTER_PARTS 100 // Max # symbols in monster
#define MAX_TILES_SQRT 12

typedef struct {
    int r;
    int c;
} Coord;

typedef struct {
    int id;
    char contents[MAX_TILE_SIZE][MAX_TILE_SIZE];
    int size;
    // Store all 8 orientations to avoid recomputing
    char orientations[8][MAX_TILE_SIZE][MAX_TILE_SIZE];
} Tile;

typedef struct {
    int tile_index; // Index into the main tiles array
    int orientation_index; // Which of the 8 orientations was used
} AssembledTileInfo;

Tile tiles[MAX_TILES];
int num_tiles = 0;
int tile_size = 0; // Assuming all tiles are the same size
int grid_edge_size = 0;

AssembledTileInfo assembled_info[MAX_TILES_SQRT][MAX_TILES_SQRT];
bool used_indices[MAX_TILES];

char image[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
int image_dim = 0;

Coord monster_offsets[MAX_MONSTER_PARTS];
int num_monster_offsets = 0;
int monster_height = 0;
int monster_width = 0;

// --- Grid Manipulation ---

void copy_grid(char dest[MAX_TILE_SIZE][MAX_TILE_SIZE], const char src[MAX_TILE_SIZE][MAX_TILE_SIZE], int size) {
    for (int i = 0; i < size; ++i) {
        memcpy(dest[i], src[i], size);
    }
}

// Rotates grid 90 degrees clockwise (out-of-place)
void rotate_grid(char dest[MAX_TILE_SIZE][MAX_TILE_SIZE], const char src[MAX_TILE_SIZE][MAX_TILE_SIZE], int size) {
    for (int i = 0; i < size; ++i) {
        for (int j = 0; j < size; ++j) {
            dest[j][size - 1 - i] = src[i][j];
        }
    }
}

// Mirrors grid horizontally (out-of-place)
void mirror_grid(char dest[MAX_TILE_SIZE][MAX_TILE_SIZE], const char src[MAX_TILE_SIZE][MAX_TILE_SIZE], int size) {
    for (int i = 0; i < size; ++i) {
        for (int j = 0; j < size; ++j) {
            dest[i][size - 1 - j] = src[i][j];
        }
    }
}

void generate_orientations(Tile *tile) {
    char temp[MAX_TILE_SIZE][MAX_TILE_SIZE];
    copy_grid(tile->orientations[0], tile->contents, tile->size); // Original

    // 3 Rotations
    copy_grid(temp, tile->orientations[0], tile->size);
    for (int i = 1; i < 4; ++i) {
        rotate_grid(tile->orientations[i], temp, tile->size);
        copy_grid(temp, tile->orientations[i], tile->size);
    }

    // Mirror original and rotate 3 times
    mirror_grid(tile->orientations[4], tile->orientations[0], tile->size);
    copy_grid(temp, tile->orientations[4], tile->size);
    for (int i = 5; i < 8; ++i) {
        rotate_grid(tile->orientations[i], temp, tile->size);
        copy_grid(temp, tile->orientations[i], tile->size);
    }
}

// --- Edge Comparison ---

bool compare_top_bottom(const char grid_above[MAX_TILE_SIZE][MAX_TILE_SIZE], const char grid_below[MAX_TILE_SIZE][MAX_TILE_SIZE], int size) {
    for (int c = 0; c < size; ++c) {
        if (grid_above[size - 1][c] != grid_below[0][c]) return false;
    }
    return true;
}

bool compare_left_right(const char grid_left[MAX_TILE_SIZE][MAX_TILE_SIZE], const char grid_right[MAX_TILE_SIZE][MAX_TILE_SIZE], int size) {
    for (int r = 0; r < size; ++r) {
        if (grid_left[r][size - 1] != grid_right[r][0]) return false;
    }
    return true;
}

// --- Backtracking Assembly ---

bool backtrack_assemble(int r, int c) {
    if (r == grid_edge_size) {
        return true; // Base case: grid filled
    }

    int next_r = r, next_c = c + 1;
    if (next_c == grid_edge_size) {
        next_r = r + 1;
        next_c = 0;
    }

    for (int i = 0; i < num_tiles; ++i) {
        if (!used_indices[i]) {
            for (int orient_idx = 0; orient_idx < 8; ++orient_idx) {
                const char (*current_grid)[MAX_TILE_SIZE] = tiles[i].orientations[orient_idx];

                // Check compatibility with tile above
                if (r > 0) {
                    int above_idx = assembled_info[r - 1][c].tile_index;
                    int above_orient = assembled_info[r - 1][c].orientation_index;
                    if (!compare_top_bottom(tiles[above_idx].orientations[above_orient], current_grid, tile_size)) {
                        continue;
                    }
                }

                // Check compatibility with tile to the left
                if (c > 0) {
                    int left_idx = assembled_info[r][c - 1].tile_index;
                    int left_orient = assembled_info[r][c - 1].orientation_index;
                    if (!compare_left_right(tiles[left_idx].orientations[left_orient], current_grid, tile_size)) {
                        continue;
                    }
                }

                // Place tile
                assembled_info[r][c].tile_index = i;
                assembled_info[r][c].orientation_index = orient_idx;
                used_indices[i] = true;

                // Recurse
                if (backtrack_assemble(next_r, next_c)) {
                    return true;
                }

                // Backtrack
                used_indices[i] = false;
                // No need to clear assembled_info[r][c], it will be overwritten
            }
        }
    }

    return false; // No suitable tile found for this position
}

// --- Image Assembly & Monster Search ---

void assemble_image() {
    int sub_size = tile_size - 2;
    image_dim = grid_edge_size * sub_size;

    for (int big_r = 0; big_r < grid_edge_size; ++big_r) {
        for (int big_c = 0; big_c < grid_edge_size; ++big_c) {
            int tile_idx = assembled_info[big_r][big_c].tile_index;
            int orient_idx = assembled_info[big_r][big_c].orientation_index;
            const char (*tile_grid)[MAX_TILE_SIZE] = tiles[tile_idx].orientations[orient_idx];

            for (int r = 0; r < sub_size; ++r) {
                for (int c = 0; c < sub_size; ++c) {
                    image[big_r * sub_size + r][big_c * sub_size + c] = tile_grid[r + 1][c + 1];
                }
            }
        }
    }
}

void setup_monster() {
    const char *monster_pattern[] = {
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    };
    monster_height = sizeof(monster_pattern) / sizeof(monster_pattern[0]);
    monster_width = strlen(monster_pattern[0]);
    num_monster_offsets = 0;
    for (int r = 0; r < monster_height; ++r) {
        for (int c = 0; c < monster_width; ++c) {
            if (monster_pattern[r][c] == '#') {
                if (num_monster_offsets < MAX_MONSTER_PARTS) {
                    monster_offsets[num_monster_offsets++] = (Coord){r, c};
                } else {
                   fprintf(stderr, "Error: Monster pattern too large\n");
                   exit(EXIT_FAILURE);
                }
            }
        }
    }
}

// Rotates a generic square grid (for the final image)
void rotate_image_grid(char dest[MAX_IMAGE_DIM][MAX_IMAGE_DIM], const char src[MAX_IMAGE_DIM][MAX_IMAGE_DIM], int dim) {
     for (int i = 0; i < dim; ++i) {
        for (int j = 0; j < dim; ++j) {
            dest[j][dim - 1 - i] = src[i][j];
        }
    }
}

// Mirrors a generic square grid (for the final image)
void mirror_image_grid(char dest[MAX_IMAGE_DIM][MAX_IMAGE_DIM], const char src[MAX_IMAGE_DIM][MAX_IMAGE_DIM], int dim) {
     for (int i = 0; i < dim; ++i) {
        for (int j = 0; j < dim; ++j) {
            dest[i][dim - 1 - j] = src[i][j];
        }
    }
}


int find_and_mark_monsters(char current_image[MAX_IMAGE_DIM][MAX_IMAGE_DIM]) {
    bool monster_found_in_image = false;
    bool is_monster_part[MAX_IMAGE_DIM][MAX_IMAGE_DIM] = {0};

    for (int r = 0; r <= image_dim - monster_height; ++r) {
        for (int c = 0; c <= image_dim - monster_width; ++c) {
            bool match = true;
            for (int i = 0; i < num_monster_offsets; ++i) {
                int mr = r + monster_offsets[i].r;
                int mc = c + monster_offsets[i].c;
                if (current_image[mr][mc] != '#') {
                    match = false;
                    break;
                }
            }
            if (match) {
                monster_found_in_image = true;
                for (int i = 0; i < num_monster_offsets; ++i) {
                     int mr = r + monster_offsets[i].r;
                     int mc = c + monster_offsets[i].c;
                     is_monster_part[mr][mc] = true;
                }
            }
        }
    }

    if (!monster_found_in_image) {
        return -1; // Indicate no monsters found in this orientation
    }

    // Count non-monster '#'
    int rough_waters = 0;
    for (int r = 0; r < image_dim; ++r) {
        for (int c = 0; c < image_dim; ++c) {
            if (current_image[r][c] == '#' && !is_monster_part[r][c]) {
                rough_waters++;
            }
        }
    }
    return rough_waters;
}


// --- Parsing ---

void parse_input(FILE *fp) {
    char line[MAX_LINE_LEN];
    int current_tile_row = 0;

    while (fgets(line, sizeof(line), fp)) {
        if (strncmp(line, "Tile ", 5) == 0) {
            if (num_tiles > 0) {
                 if (current_tile_row != tiles[num_tiles-1].size) {
                    fprintf(stderr, "Error: Inconsistent tile size at tile %d\n", tiles[num_tiles-1].id);
                    exit(EXIT_FAILURE);
                 }
            }
            sscanf(line, "Tile %d:", &tiles[num_tiles].id);
            current_tile_row = 0;
        } else if (line[0] == '\n' || line[0] == '\r') {
            // End of a tile block
             if (current_tile_row > 0) { // Make sure we read some rows for the last tile
                 tiles[num_tiles].size = current_tile_row;
                 if (tile_size == 0) tile_size = current_tile_row;
                 else if (tile_size != current_tile_row) {
                     fprintf(stderr, "Error: Inconsistent tile sizes (%d vs %d)\n", tile_size, current_tile_row);
                     exit(EXIT_FAILURE);
                 }
                 generate_orientations(&tiles[num_tiles]);
                 num_tiles++;
                 if (num_tiles >= MAX_TILES) {
                     fprintf(stderr, "Error: Exceeded MAX_TILES\n");
                     exit(EXIT_FAILURE);
                 }
             }
        } else {
            // Reading tile contents
            int len = strlen(line);
            // Remove trailing newline/carriage return
            while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
                line[--len] = '\0';
            }

            if (current_tile_row < MAX_TILE_SIZE && len > 0) {
                if (current_tile_row == 0 && tile_size == 0) {
                    tile_size = len; // Set expected size based on first row
                    if (tile_size > MAX_TILE_SIZE) {
                        fprintf(stderr, "Error: Tile size %d exceeds MAX_TILE_SIZE %d\n", tile_size, MAX_TILE_SIZE);
                        exit(EXIT_FAILURE);
                    }
                } else if (len != tile_size) {
                     fprintf(stderr, "Error: Inconsistent row length in tile %d (expected %d, got %d)\n", tiles[num_tiles].id, tile_size, len);
                     exit(EXIT_FAILURE);
                }

                memcpy(tiles[num_tiles].contents[current_tile_row], line, tile_size);
                current_tile_row++;
            }
        }
    }
     // Process the very last tile if the file doesn't end with a blank line
    if (current_tile_row > 0) {
         tiles[num_tiles].size = current_tile_row;
         if (tile_size == 0) tile_size = current_tile_row;
          else if (tile_size != current_tile_row) {
              fprintf(stderr, "Error: Inconsistent tile sizes (%d vs %d) for last tile\n", tile_size, current_tile_row);
              exit(EXIT_FAILURE);
          }
         generate_orientations(&tiles[num_tiles]);
         num_tiles++;
     }

    // Calculate grid edge size
    double sqrt_tiles = sqrt((double)num_tiles);
    if (sqrt_tiles != floor(sqrt_tiles) || num_tiles == 0) {
         fprintf(stderr, "Error: Number of tiles (%d) is not a perfect square or is zero.\n", num_tiles);
         exit(EXIT_FAILURE);
    }
    grid_edge_size = (int)sqrt_tiles;
     if (grid_edge_size > MAX_TILES_SQRT) {
        fprintf(stderr, "Error: Grid edge size %d exceeds MAX_TILES_SQRT %d\n", grid_edge_size, MAX_TILES_SQRT);
        exit(EXIT_FAILURE);
    }
    int expected_image_dim = grid_edge_size * (tile_size - 2);
     if (expected_image_dim > MAX_IMAGE_DIM) {
         fprintf(stderr, "Error: Calculated image dimension %d exceeds MAX_IMAGE_DIM %d\n", expected_image_dim, MAX_IMAGE_DIM);
        exit(EXIT_FAILURE);
    }
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    parse_input(fp);
    fclose(fp);

    if (tile_size == 0) {
        fprintf(stderr, "Error: No tiles parsed or tile size is zero.\n");
        return EXIT_FAILURE;
    }

    setup_monster();

    // Initialize used flags
    memset(used_indices, 0, sizeof(used_indices));

    // Assemble the tile grid
    if (!backtrack_assemble(0, 0)) {
        fprintf(stderr, "Error: Failed to assemble tiles.\n");
        return EXIT_FAILURE;
    }

    // Create the final image from assembled tiles (without borders)
    assemble_image();

    // Try all 8 orientations of the final image to find the monster
    char current_image_orient[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
    char temp_image[MAX_IMAGE_DIM][MAX_IMAGE_DIM];
    int final_roughness = -1;

    // Copy initial image state
     for(int i=0; i<image_dim; ++i) memcpy(current_image_orient[i], image[i], image_dim);

    for (int mirror = 0; mirror < 2; ++mirror) {
        for (int rot = 0; rot < 4; ++rot) {
            int roughness = find_and_mark_monsters(current_image_orient);
            if (roughness != -1) {
                final_roughness = roughness;
                goto found_monsters; // Exit loops once monsters are found
            }
            // Rotate for next iteration
            rotate_image_grid(temp_image, current_image_orient, image_dim);
            for(int i=0; i<image_dim; ++i) memcpy(current_image_orient[i], temp_image[i], image_dim);
        }
        // Mirror after 4 rotations
        mirror_image_grid(temp_image, image, image_dim); // Mirror the original image state
        for(int i=0; i<image_dim; ++i) memcpy(current_image_orient[i], temp_image[i], image_dim);
        // Also update the base 'image' state for the next mirror's rotations if needed
        // (Actually, mirroring the original 'image' is sufficient)
         for(int i=0; i<image_dim; ++i) memcpy(image[i], temp_image[i], image_dim);
    }


found_monsters:
    if (final_roughness == -1) {
        fprintf(stderr, "Error: Sea monsters not found in any orientation.\n");
        return EXIT_FAILURE;
    }

    printf("%d\n", final_roughness);

    return EXIT_SUCCESS;
}

