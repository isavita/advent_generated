
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

char* read_algorithm(FILE* file) {
    char* algorithm = malloc(513 * sizeof(char));
    if (fgets(algorithm, 513, file) == NULL) {
        free(algorithm);
        return NULL;
    }
    algorithm[strcspn(algorithm, "\n")] = 0;
    return algorithm;
}

char** read_image(FILE* file, int* rows, int* cols) {
    char** image = NULL;
    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    *rows = 0;
    *cols = 0;

    while ((read = getline(&line, &len, file)) != -1) {
      if (read <= 1) continue;
        line[strcspn(line, "\n")] = 0;
        int current_cols = strlen(line);
         if (*rows == 0) {
             *cols = current_cols;
         } else if (*cols != current_cols){
          free(line);
          for(int i = 0; i < *rows; ++i){
              free(image[i]);
            }
            free(image);
           return NULL;
         }
        image = realloc(image, (*rows + 1) * sizeof(char*));
        image[*rows] = malloc((current_cols + 1) * sizeof(char));
        strcpy(image[*rows], line);
        (*rows)++;
    }
    free(line);
    return image;
}


int calculate_index(int i, int j, char** image, int rows, int cols, bool flip) {
    int index = 0;
    for (int di = -1; di <= 1; di++) {
        for (int dj = -1; dj <= 1; dj++) {
            index <<= 1;
            int ni = i + di;
            int nj = j + dj;
            if (ni >= 0 && ni < rows && nj >= 0 && nj < cols) {
                if (image[ni][nj] == '#') {
                    index |= 1;
                }
            } else if (flip) {
                index |= 1;
            }
        }
    }
    return index;
}

char** apply_algorithm(char** image, int rows, int cols, char* algorithm, bool flip,int *new_rows, int *new_cols) {
    *new_rows = rows + 2;
    *new_cols = cols + 2;
    char** enhanced_image = malloc(*new_rows * sizeof(char*));
    for (int i = 0; i < *new_rows; i++) {
        enhanced_image[i] = malloc(*new_cols * sizeof(char));
        for (int j = 0; j < *new_cols; j++) {
            int index = calculate_index(i - 1, j - 1, image, rows, cols, flip);
            enhanced_image[i][j] = algorithm[index];
        }
    }
    return enhanced_image;
}

char** enhance_image(char** image, int rows, int cols, char* algorithm, int times, int *new_rows, int *new_cols) {
    for (int i = 0; i < times; i++) {
        bool flip = (i % 2 == 1) && algorithm[0] == '#';
        char** temp = apply_algorithm(image,rows,cols,algorithm,flip, new_rows, new_cols);
        if (image != NULL){
          for (int r = 0; r<rows; ++r){
           free(image[r]);
          }
          free(image);
        }
        image = temp;
         rows = *new_rows;
         cols = *new_cols;
    }
    return image;
}

int count_lit_pixels(char** image, int rows, int cols) {
    int count = 0;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (image[i][j] == '#') {
                count++;
            }
        }
    }
    return count;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char* algorithm = read_algorithm(file);
    if (algorithm == NULL) {
      fclose(file);
      return 1;
    }
    
    int rows, cols;
    char** image = read_image(file, &rows, &cols);
    fclose(file);
    if (image == NULL){
        free(algorithm);
        return 1;
    }
   
    int new_rows, new_cols;
    image = enhance_image(image, rows, cols, algorithm, 2, &new_rows, &new_cols);
    
    printf("%d\n", count_lit_pixels(image, new_rows, new_cols));
    
    for (int i = 0; i < new_rows; ++i){
        free(image[i]);
    }
    free(image);
    free(algorithm);

    return 0;
}
