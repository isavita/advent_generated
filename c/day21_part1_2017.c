
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 256
#define MAX_GRID_SIZE 200
#define MAX_RULES 1000
#define MAX_RULE_LENGTH 30

typedef struct {
    char input[MAX_RULE_LENGTH];
    char output[MAX_RULE_LENGTH];
} Rule;

Rule rules[MAX_RULES];
int num_rules = 0;

char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
int grid_size = 3;

void read_rules() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening input.txt");
        exit(EXIT_FAILURE);
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), fp) != NULL) {
        char *arrow = strstr(line, " => ");
        if (arrow == NULL) continue;

        *arrow = '\0';
        strcpy(rules[num_rules].input, line);
        strcpy(rules[num_rules].output, arrow + 4);
        rules[num_rules].output[strcspn(rules[num_rules].output, "\n")] = '\0';
        num_rules++;
    }
    fclose(fp);
}


void rotate(char *input, int size, char *output) {
    char parts[size][size];
    int k = 0;
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            parts[i][j] = input[k++];
        }
        k++;
    }
    k = 0;
    for (int x = 0; x < size; x++) {
        for (int y = size - 1; y >= 0; y--) {
            output[k++] = parts[y][x];
        }
         output[k++] = '/';
    }
    output[k-1] = '\0';
}


void flip(char *input, int size, char *output) {
  int k = 0;
  char parts[size][size];
  for (int i = 0; i < size; i++) {
    for (int j = 0; j < size; j++) {
      parts[i][j] = input[k++];
    }
    k++;
  }
  k = 0;
  for(int i = 0; i < size; i++){
    for(int j = size -1; j >= 0; j--){
      output[k++] = parts[i][j];
    }
    output[k++] = '/';
  }
  output[k-1] = '\0';

}


char *enhance(char *input, int size) {
    char rotated[MAX_RULE_LENGTH];
    char flipped[MAX_RULE_LENGTH];
    
    
     for (int i = 0; i < 4; i++) {
         for(int r = 0; r < num_rules; r++){
             if(strcmp(input, rules[r].input) == 0){
                 return rules[r].output;
             }
         }
         rotate(input,size, rotated);
         strcpy(input, rotated);
    }
    flip(input, size, flipped);
    strcpy(input, flipped);

    for (int i = 0; i < 4; i++) {
        for(int r = 0; r < num_rules; r++){
             if(strcmp(input, rules[r].input) == 0){
                 return rules[r].output;
             }
         }
        rotate(input, size, rotated);
        strcpy(input, rotated);
    }
    return NULL;
}


void apply_rules() {
    int new_size;
    int sub_size;

    if (grid_size % 2 == 0) {
        sub_size = 2;
        new_size = grid_size / 2 * 3;
    } else {
        sub_size = 3;
        new_size = grid_size / 3 * 4;
    }

    char new_grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
    for (int i = 0; i < new_size; i++) {
        for (int j = 0; j < new_size; j++) {
          new_grid[i][j] = 0;
        }
    }


    for (int y = 0; y < grid_size; y += sub_size) {
        for (int x = 0; x < grid_size; x += sub_size) {
            char square[MAX_RULE_LENGTH] = "";
            int k = 0;
           for (int dy = 0; dy < sub_size; dy++) {
               for(int dx = 0; dx < sub_size; dx++){
                   square[k++] = grid[y + dy][x + dx];
                }
                square[k++] = '/';
             }
            square[k-1] = '\0';

            char *new_square = enhance(square, sub_size);
            int row = y / sub_size * (sub_size + 1);
           
            k = 0;
            if(new_square != NULL){
              for(int dy = 0; dy < sub_size + 1; dy++){
                  for(int dx = 0; dx < sub_size + 1; dx++){
                    new_grid[row+dy][x / sub_size * (sub_size + 1) + dx] = new_square[k++];
                   }
                 k++;
                }
            }
        }
    }

    for (int i = 0; i < new_size; i++) {
        for (int j = 0; j < new_size; j++) {
          grid[i][j] = new_grid[i][j];
        }
    }
    grid_size = new_size;
}


int main() {
    read_rules();
    
    strcpy(grid[0], ".#.");
    strcpy(grid[1], "..#");
    strcpy(grid[2], "###");


    for (int i = 0; i < 5; i++) {
        apply_rules();
    }

    int count = 0;
    for (int i = 0; i < grid_size; i++) {
        for (int j = 0; j < grid_size; j++) {
            if (grid[i][j] == '#') {
                count++;
            }
        }
    }
    printf("%d\n", count);

    return 0;
}
