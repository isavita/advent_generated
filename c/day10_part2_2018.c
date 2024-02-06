
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Star {
    int x;
    int y;
    int vX;
    int vY;
    struct Star* next;
};

int toInt(char* s) {
    return atoi(s);
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    struct Star* head = NULL;
    struct Star* tail = NULL;

    while (fgets(line, sizeof(line), file)) {
        int x, y, vX, vY;
        if (sscanf(line, "position=<%d, %d> velocity=<%d, %d>", &x, &y, &vX, &vY) == 4) {
            struct Star* star = (struct Star*)malloc(sizeof(struct Star));
            star->x = x;
            star->y = y;
            star->vX = vX;
            star->vY = vY;
            star->next = NULL;

            if (head == NULL) {
                head = star;
                tail = star;
            } else {
                tail->next = star;
                tail = star;
            }
        }
    }

    int smallestT = 0;
    int smallestArea = ~(1 << 31);
    for (int t = 1; t < 100000; t++) {
        int maxX = 0;
        int maxY = 0;
        int minX = 0;
        int minY = 0;

        for (struct Star* temp = head->next; temp->next != NULL; temp = temp->next) {
            int x = temp->x + temp->vX * t;
            if (maxX < x) {
                maxX = x;
            } else if (minX > x) {
                minX = x;
            }
            int y = temp->y + temp->vY * t;
            if (maxY < y) {
                maxY = y;
            } else if (minY > y) {
                minY = y;
            }
        }

        int lenX = maxX - minY + 1;
        int lenY = maxY - minY + 1;
        int area = lenX + lenY;

        if (smallestArea > area) {
            smallestArea = area;
            smallestT = t;
        }
    }

    printf("%d\n", smallestT);

    int t = smallestT;

    int maxX = 0;
    int maxY = 0;
    int minX = 0;
    int minY = 0;

    for (struct Star* temp = head->next; temp->next != NULL; temp = temp->next) {
        temp->x = temp->x + temp->vX * t;
        if (maxX < temp->x) {
            maxX = temp->x;
        } else if (minX > temp->x) {
            minX = temp->x;
        }
        temp->y = temp->y + temp->vY * t;
        if (maxY < temp->y) {
            maxY = temp->y;
        } else if (minY > temp->y) {
            minY = temp->y;
        }
    }

    int height = maxY - minY + 1;
    int width = maxX - minX + 1;
    int** mapper = (int**)malloc(sizeof(int*) * height);
    for (int i = 0; i < height; i++) {
        mapper[i] = (int*)malloc(sizeof(int) * width);
        memset(mapper[i], 0, sizeof(int) * width);
    }

    for (struct Star* temp = head->next; temp->next != NULL; temp = temp->next) {
        mapper[temp->y - minY][temp->x - minX] = 1;
    }

    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            // Do something with mapper[i][j]
        }
    }

    for (int i = 0; i < height; i++) {
        free(mapper[i]);
    }
    free(mapper);

    fclose(file);

    return 0;
}
