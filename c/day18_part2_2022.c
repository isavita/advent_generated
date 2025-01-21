
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

typedef struct {
    int x, y, z;
} Point3D;

typedef struct {
    Point3D *data;
    size_t capacity;
    size_t size;
} PointQueue;

void queue_init(PointQueue *queue) {
    queue->data = NULL;
    queue->capacity = 0;
    queue->size = 0;
}

void queue_push(PointQueue *queue, Point3D p) {
    if (queue->size == queue->capacity) {
        queue->capacity = (queue->capacity == 0) ? 1 : queue->capacity * 2;
        queue->data = realloc(queue->data, queue->capacity * sizeof(Point3D));
        if (queue->data == NULL) {
            perror("Memory allocation failed");
            exit(EXIT_FAILURE);
        }
    }
    queue->data[queue->size++] = p;
}

Point3D queue_pop(PointQueue *queue) {
    if (queue->size == 0) {
        fprintf(stderr, "Queue is empty\n");
        exit(EXIT_FAILURE);
    }
    Point3D p = queue->data[0];
    memmove(queue->data, queue->data + 1, (queue->size - 1) * sizeof(Point3D));
    queue->size--;
    return p;
}

bool queue_is_empty(const PointQueue *queue) {
    return queue->size == 0;
}

void queue_free(PointQueue *queue) {
    free(queue->data);
    queue->data = NULL;
    queue->size = 0;
    queue->capacity = 0;
}

int min(int a, int b) {
    return (a < b) ? a : b;
}

int max(int a, int b) {
    return (a > b) ? a : b;
}

Point3D add_points(Point3D p1, Point3D p2) {
    Point3D result;
    result.x = p1.x + p2.x;
    result.y = p1.y + p2.y;
    result.z = p1.z + p2.z;
    return result;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    int cube_count = 0;
    Point3D cubes[10000]; 
    Point3D min_pt = {INT_MAX, INT_MAX, INT_MAX};
    Point3D max_pt = {INT_MIN, INT_MIN, INT_MIN};

    char line[256];
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (line[0] == '\n') continue;
        
        if (sscanf(line, "%d,%d,%d", &cubes[cube_count].x, &cubes[cube_count].y, &cubes[cube_count].z) == 3) {
             min_pt.x = min(min_pt.x, cubes[cube_count].x);
             min_pt.y = min(min_pt.y, cubes[cube_count].y);
             min_pt.z = min(min_pt.z, cubes[cube_count].z);
             max_pt.x = max(max_pt.x, cubes[cube_count].x);
             max_pt.y = max(max_pt.y, cubes[cube_count].y);
             max_pt.z = max(max_pt.z, cubes[cube_count].z);
             cube_count++;
        }
    }
    fclose(fp);

    min_pt.x -= 1;
    min_pt.y -= 1;
    min_pt.z -= 1;
    max_pt.x += 1;
    max_pt.y += 1;
    max_pt.z += 1;

    int faces = 0;
    PointQueue queue;
    queue_init(&queue);
    queue_push(&queue, min_pt);

    bool seen[100][100][100] = {false};
    seen[min_pt.x - min_pt.x][min_pt.y - min_pt.y][min_pt.z-min_pt.z] = true;

    Point3D neighbors[] = {{-1, 0, 0}, {1, 0, 0}, {0, -1, 0}, {0, 1, 0}, {0, 0, -1}, {0, 0, 1}};

    while (!queue_is_empty(&queue)) {
        Point3D curr = queue_pop(&queue);

        for (int i = 0; i < 6; i++) {
            Point3D next = add_points(curr, neighbors[i]);

            if (next.x < min_pt.x || next.y < min_pt.y || next.z < min_pt.z ||
                next.x > max_pt.x || next.y > max_pt.y || next.z > max_pt.z) {
                continue;
            }

            bool is_cube = false;
            for (int j = 0; j < cube_count; j++) {
               if (cubes[j].x == next.x && cubes[j].y == next.y && cubes[j].z == next.z) {
                    is_cube = true;
                    break;
                }
            }


            if (is_cube) {
                faces++;
            } else if (!seen[next.x - min_pt.x][next.y - min_pt.y][next.z - min_pt.z]) {
                seen[next.x - min_pt.x][next.y - min_pt.y][next.z-min_pt.z] = true;
                queue_push(&queue, next);
            }
        }
    }
     queue_free(&queue);
    printf("%d\n", faces);

    return EXIT_SUCCESS;
}
