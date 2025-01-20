
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    char **data;
    int rows;
    int cols;
} Matrix;

Matrix read_file_to_matrix(const char *file_path);
int sum_of_part_numbers(Matrix matrix);
int extract_number(Matrix matrix, int x, int y, int *length);
bool is_adjacent_to_symbol(Matrix matrix, int x, int y, int length);
bool check_adjacent(Matrix matrix, int x, int y);
void free_matrix(Matrix matrix);

int main() {
    Matrix matrix = read_file_to_matrix("input.txt");
    if (matrix.data == NULL) {
        fprintf(stderr, "Error reading file.\n");
        return 1;
    }
    
    int sum = sum_of_part_numbers(matrix);
    printf("%d\n", sum);

    free_matrix(matrix);
    return 0;
}

Matrix read_file_to_matrix(const char *file_path) {
    FILE *file = fopen(file_path, "r");
    Matrix matrix = {NULL, 0, 0};
    if (!file) return matrix;

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int rows = 0;
    int cols = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0'; 
            read--;
        }
         if(rows == 0) {
             cols = read;
         } else if(cols != read) {
            fprintf(stderr, "Error: inconsistent line length\n");
            fclose(file);
            if(line) free(line);
            return (Matrix) {NULL, 0, 0};

         }

        matrix.data = realloc(matrix.data, (rows + 1) * sizeof(char*));
        if (!matrix.data) {
            fprintf(stderr, "Memory allocation error\n");
            fclose(file);
            if (line) free(line);
            return (Matrix) {NULL, 0, 0};
        }
        matrix.data[rows] = strdup(line);
         if(!matrix.data[rows]){
             fprintf(stderr, "Memory allocation error\n");
             fclose(file);
             if (line) free(line);
             free_matrix(matrix);
             return (Matrix) {NULL, 0, 0};

         }
        rows++;
    }

    free(line);
    fclose(file);
    matrix.rows = rows;
    matrix.cols = cols;

    return matrix;
}

int sum_of_part_numbers(Matrix matrix) {
    int sum = 0;
    int rows = matrix.rows;
    int cols = matrix.cols;
    bool *visited = calloc(rows * cols, sizeof(bool));

    if(!visited){
      fprintf(stderr,"Memory allocation error\n");
      return 0;
    }
    
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (!visited[y * cols + x] && isdigit(matrix.data[y][x])) {
                int length;
                int number = extract_number(matrix, x, y, &length);
                if (is_adjacent_to_symbol(matrix, x, y, length)) {
                    sum += number;
                }
                for (int i = 0; i < length; i++) {
                    visited[y * cols + x + i] = true;
                }
                x += length -1;
            }
        }
    }
    free(visited);
    return sum;
}


int extract_number(Matrix matrix, int x, int y, int *length) {
    int num = 0;
    *length = 0;
    while (x < matrix.cols && isdigit(matrix.data[y][x])) {
        num = num * 10 + (matrix.data[y][x] - '0');
        x++;
        (*length)++;
    }
    return num;
}


bool is_adjacent_to_symbol(Matrix matrix, int x, int y, int length) {
    for (int i = 0; i < length; i++) {
        if (check_adjacent(matrix, x + i, y)) {
            return true;
        }
    }
    return false;
}

bool check_adjacent(Matrix matrix, int x, int y) {
  int rows = matrix.rows;
  int cols = matrix.cols;
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx == 0 && dy == 0) continue;
            int adj_x = x + dx;
            int adj_y = y + dy;
            if (adj_y >= 0 && adj_y < rows && adj_x >= 0 && adj_x < cols) {
                if (!isdigit(matrix.data[adj_y][adj_x]) && matrix.data[adj_y][adj_x] != '.') {
                    return true;
                }
            }
        }
    }
    return false;
}

void free_matrix(Matrix matrix) {
  if(matrix.data){
    for (int i = 0; i < matrix.rows; i++) {
        free(matrix.data[i]);
    }
    free(matrix.data);
  }
}
