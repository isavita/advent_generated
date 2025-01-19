
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point point;
    int steps;
    struct Node* next;
} Node;

Node* createNode(Point p, int steps) {
    Node* newNode = (Node*)malloc(sizeof(Node));
    if (newNode == NULL) {
        perror("Failed to allocate memory");
        exit(1);
    }
    newNode->point = p;
    newNode->steps = steps;
    newNode->next = NULL;
    return newNode;
}

void insert(Node** head, Point p, int steps) {
    Node* newNode = createNode(p, steps);
    if (*head == NULL) {
        *head = newNode;
    } else {
        Node* temp = *head;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        temp->next = newNode;
    }
}

int findSteps(Node* head, Point p) {
    Node* temp = head;
    while (temp != NULL) {
        if (temp->point.x == p.x && temp->point.y == p.y) {
            return temp->steps;
        }
        temp = temp->next;
    }
    return -1;
}

void freeList(Node* head) {
    Node* temp;
    while (head != NULL) {
        temp = head;
        head = head->next;
        free(temp);
    }
}


Node* getPointsWithSteps(char* path) {
    Node* points = NULL;
    Point current = {0, 0};
    int steps = 0;
    char *token = strtok(path, ",");
    while (token != NULL) {
        char dir = token[0];
        int dist = atoi(token + 1);
        for (int i = 0; i < dist; i++) {
            steps++;
            switch (dir) {
                case 'U':
                    current.y++;
                    break;
                case 'D':
                    current.y--;
                    break;
                case 'L':
                    current.x--;
                    break;
                case 'R':
                    current.x++;
                    break;
            }
            if(findSteps(points,current) == -1){
               insert(&points, current, steps);
            }
        }
         token = strtok(NULL, ",");
    }
    return points;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line1[2048], line2[2048];
    if (fgets(line1, sizeof(line1), fp) == NULL || fgets(line2, sizeof(line2), fp) == NULL) {
        perror("Error reading file");
        fclose(fp);
        return 1;
    }
    fclose(fp);

    line1[strcspn(line1, "\n")] = 0;
    line2[strcspn(line2, "\n")] = 0;

    Node* wire1 = getPointsWithSteps(line1);
    Node* wire2 = getPointsWithSteps(line2);

    int minSteps = INT_MAX;
    Node* temp1 = wire1;
    while (temp1 != NULL) {
        int steps2 = findSteps(wire2, temp1->point);
        if (steps2 != -1) {
            int totalSteps = temp1->steps + steps2;
            if (totalSteps < minSteps) {
                minSteps = totalSteps;
            }
        }
        temp1 = temp1->next;
    }

    printf("%d\n", minSteps);

    freeList(wire1);
    freeList(wire2);

    return 0;
}
