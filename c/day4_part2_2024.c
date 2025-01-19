
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool checkMAS(char** grid, int rows, int cols, int x, int y, int dx, int dy) {
    if (x < 0 || y < 0 || x >= rows || y >= cols) return false;
    char word[] = "MAS";
    bool forward = true, backward = true;
    for (int i = 0; i < 3; i++) {
        int nx = x + dx * i, ny = y + dy * i;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i]) forward = false;
		if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[2 - i]) backward = false;
    }
    return forward || backward;
}

bool checkXMAS(char** grid, int rows, int cols, int x, int y) {
    return (checkMAS(grid, rows, cols, x - 1, y - 1, 1, 1) && checkMAS(grid, rows, cols, x - 1, y + 1, 1, -1)) ||
           (checkMAS(grid, rows, cols, x + 1, y - 1, -1, 1) && checkMAS(grid, rows, cols, x + 1, y + 1, -1, -1));
}

int countXMASPatterns(char** grid, int rows, int cols) {
    if (rows < 3 || cols < 3) return 0;
    int count = 0;
    for (int i = 1; i < rows - 1; i++) {
        for (int j = 1; j < cols - 1; j++) {
            if (grid[i][j] == 'A' && checkXMAS(grid, rows, cols, i, j)) count++;
        }
    }
    return count;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char** grid = NULL;
    int rows = 0, cols = 0, capacity = 0;
    char* line = NULL;
    size_t len = 0;
	ssize_t read;
    while ((read = getline(&line, &len, file)) != -1) {
        if (read > 1) {
            if (rows == capacity) {
                capacity = capacity == 0 ? 1 : capacity * 2;
                grid = realloc(grid, sizeof(char*) * capacity);
				if(!grid){
					perror("Memory Error");
					free(line);
					return 1;
				}
            }
			if(line[read-1] == '\n') line[read-1] = '\0';
            grid[rows] = strdup(line);
			if(!grid[rows]){
				perror("Memory Error");
				free(line);
				for(int i = 0; i < rows; i++){
					free(grid[i]);
				}
				free(grid);
				return 1;
			}
            if (rows == 0) cols = strlen(grid[rows]);
            rows++;
        }
    }
    free(line);
    fclose(file);
    int count = countXMASPatterns(grid, rows, cols);
    printf("X-MAS patterns appear %d times in the word search\n", count);

    for(int i = 0; i < rows; i++) free(grid[i]);
    free(grid);
    return 0;
}
