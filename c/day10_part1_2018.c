#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef struct Star {
    int x, y, vX, vY;
    struct Star* next;
} Star;

int toInt(char* s) {
    return atoi(s);
}

int main() {
    FILE* file = fopen("input.txt", "r");
    char line[100];
    Star* head = malloc(sizeof(Star));
    Star* tail = head;
    
    while (fgets(line, sizeof(line), file)) {
        int x, y, vX, vY;
        if (sscanf(line, " position=< %d, %d> velocity=< %d, %d>", &x, &y, &vX, &vY) == 4) {
            Star* star = malloc(sizeof(Star));
            star->x = x;
            star->y = y;
            star->vX = vX;
            star->vY = vY;
            tail->next = star;
            tail = star;
        }
    }
    fclose(file);
    
    tail->next = NULL;
    int smallestT = 0, smallestArea = INT_MAX;

    for (int t = 1; t < 100000; t++) {
        int maxX = INT_MIN, maxY = INT_MIN, minX = INT_MAX, minY = INT_MAX;

        for (Star* temp = head->next; temp; temp = temp->next) {
            int x = temp->x + temp->vX * t;
            int y = temp->y + temp->vY * t;
            if (x > maxX) maxX = x;
            if (y > maxY) maxY = y;
            if (x < minX) minX = x;
            if (y < minY) minY = y;
        }

        int area = (maxX - minX + 1) + (maxY - minY + 1);
        if (smallestArea > area) {
            smallestArea = area;
            smallestT = t;
        }
    }

    for (Star* temp = head->next; temp; temp = temp->next) {
        temp->x += temp->vX * smallestT;
        temp->y += temp->vY * smallestT;
    }

    int maxX = INT_MIN, maxY = INT_MIN, minX = INT_MAX, minY = INT_MAX;

    for (Star* temp = head->next; temp; temp = temp->next) {
        if (temp->x > maxX) maxX = temp->x;
        if (temp->y > maxY) maxY = temp->y;
        if (temp->x < minX) minX = temp->x;
        if (temp->y < minY) minY = temp->y;
    }

    char** mapper = malloc((maxY - minY + 1) * sizeof(char*));
    for (int i = 0; i <= maxY - minY; i++) {
        mapper[i] = calloc(maxX - minX + 1, sizeof(char));
        memset(mapper[i], ' ', maxX - minX + 1);
    }

    for (Star* temp = head->next; temp; temp = temp->next) {
        mapper[temp->y - minY][temp->x - minX] = '#';
    }

    for (int i = 0; i <= maxY - minY; i++) {
        printf("%s\n", mapper[i]);
        free(mapper[i]);
    }
    free(mapper);
    
    Star* current = head;
    while (current) {
        Star* next = current->next;
        free(current);
        current = next;
    }
    
    return 0;
}