
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    long long x;
    long long y;
} Coord;

Coord coord_add(Coord c1, Coord c2) {
    Coord res = {c1.x + c2.x, c1.y + c2.y};
    return res;
}

Coord coord_multiply_scalar(Coord c, long long s) {
    Coord res = {c.x * s, c.y * s};
    return res;
}

long long abs_val(long long x) {
    return x < 0 ? -x : x;
}

long long hex_string_to_int(const char *hex_str) {
    long long res = 0;
    for (int i = 0; hex_str[i] != '\0'; i++) {
        res *= 16;
        if (isdigit(hex_str[i])) {
            res += hex_str[i] - '0';
        } else if (hex_str[i] >= 'a' && hex_str[i] <= 'f') {
            res += hex_str[i] - 'a' + 10;
        }
    }
    return res;
}

Coord* parse_input(char **input, int input_len, int *vertices_len) {
    const char Up = '3', Left = '2', Down = '1', Right = '0';
    Coord current = {0, 0};
    Coord *vertices = malloc(sizeof(Coord) * (input_len + 1));
    if (vertices == NULL) {
      perror("malloc failed");
      exit(EXIT_FAILURE);
    }
    vertices[0] = current;
    *vertices_len = 1;

    for (int i = 0; i < input_len; i++) {
        char *line = input[i];
        char *parts[3];
        char *token = strtok(line, " ");
        int part_count = 0;
        while (token != NULL && part_count < 3) {
            parts[part_count++] = token;
            token = strtok(NULL, " ");
        }
        char *color = parts[2];
        char dir_input = color[7];
        char length_str[6];
        strncpy(length_str, &color[2], 5);
        length_str[5] = '\0';
        long long length = hex_string_to_int(length_str);

        Coord dir;
        switch (dir_input) {
            case Up:
                dir = (Coord){0, -1};
                break;
            case Left:
                dir = (Coord){-1, 0};
                break;
            case Down:
                dir = (Coord){0, 1};
                break;
            case Right:
                dir = (Coord){1, 0};
                break;
             default:
                fprintf(stderr, "Invalid direction input: %c\n", dir_input);
                free(vertices);
                exit(EXIT_FAILURE);
        }
        current = coord_add(current, coord_multiply_scalar(dir, length));
        vertices[(*vertices_len)++] = current;
    }

    return vertices;
}

long long shoelace(Coord *vertices, int n) {
    long long area = 0;
    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        area += vertices[i].x * vertices[next].y;
        area -= vertices[i].y * vertices[next].x;
    }
    return abs_val(area) / 2;
}

long long perimeter(Coord *vertices, int n) {
    long long perim = 0;
    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        perim += abs_val(vertices[i].x - vertices[next].x) + abs_val(vertices[i].y - vertices[next].y);
    }
    return perim;
}

long long calculate_polygon_area(Coord *vertices, int n) {
    return shoelace(vertices, n) + perimeter(vertices, n) / 2 + 1;
}


int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    char **input = NULL;
    int input_len = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
      if (line[read -1] == '\n'){
        line[read -1] = '\0';
      }
      input = realloc(input, sizeof(char*) * (input_len + 1));
      if (input == NULL){
        perror("realloc failed");
        free(line);
        fclose(fp);
        return EXIT_FAILURE;
      }
      input[input_len] = strdup(line);
        if (input[input_len] == NULL) {
          perror("strdup failed");
          free(line);
          for(int i =0 ; i < input_len; i++){
              free(input[i]);
          }
          free(input);
          fclose(fp);
          return EXIT_FAILURE;
        }
      input_len++;
    }
    free(line);
    fclose(fp);
    int vertices_len;
    Coord *vertices = parse_input(input, input_len, &vertices_len);
    long long result = calculate_polygon_area(vertices, vertices_len);
    printf("%lld\n", result);
    free(vertices);
    for (int i = 0; i < input_len; i++){
      free(input[i]);
    }
    free(input);
    return EXIT_SUCCESS;
}
