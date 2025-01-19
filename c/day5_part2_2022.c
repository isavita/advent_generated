
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_STACKS 10
#define MAX_HEIGHT 100

typedef struct {
    char data[MAX_HEIGHT];
    int top;
} Stack;

void stack_push(Stack *s, char item) {
    s->data[++(s->top)] = item;
}

char stack_pop(Stack *s) {
    return s->data[(s->top)--];
}

int stack_size(Stack *s) {
    return s->top + 1;
}

void move(Stack stacks[], int num_stacks, int n, int from, int to) {
  char temp[n];
  for (int i = 0; i < n; i++) {
     temp[i] = stack_pop(&stacks[from]);
  }
    for (int i = n -1; i >=0; i--) {
        stack_push(&stacks[to], temp[i]);
    }

}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    Stack stacks[MAX_STACKS];
    for (int i = 0; i < MAX_STACKS; i++) {
      stacks[i].top = -1;
    }
    int num_stacks = 0;
    
    while ((read = getline(&line, &len, fp)) != -1) {
        if (line[1] == '1')
             break;
      
        for (int i = 1; i < read; i += 4) {
           if (isalpha(line[i])) {
            stack_push(&stacks[i/4], line[i]);
            if (i/4 + 1 > num_stacks)
                num_stacks = i/4 + 1;
            }
        }
    }
    
    for(int i=0; i < num_stacks; i++) {
        int size = stack_size(&stacks[i]);
        char tmp[size];
        for(int j=0; j<size; j++)
            tmp[j] = stack_pop(&stacks[i]);
        for(int j=0; j<size; j++)
            stack_push(&stacks[i], tmp[j]);
    }
    
    
    while ((read = getline(&line, &len, fp)) != -1) {
        if (line[0] == 'm') {
             int n, from, to;
                sscanf(line, "move %d from %d to %d", &n, &from, &to);
                move(stacks, num_stacks, n, from - 1, to - 1);
        }
    }

    for (int i = 0; i < num_stacks; i++) {
        printf("%c", stacks[i].data[stacks[i].top]);
    }
    printf("\n");
    
    if(line) free(line);
    fclose(fp);
    return 0;
}
