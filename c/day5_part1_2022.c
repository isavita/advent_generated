
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

void stack_init(Stack *s) {
    s->top = -1;
}

void stack_push(Stack *s, char c) {
    s->data[++s->top] = c;
}

char stack_pop(Stack *s) {
    return s->data[s->top--];
}

char stack_peek(Stack *s) {
    return s->data[s->top];
}

int stack_empty(Stack *s) {
    return s->top == -1;
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
    for(int i=0; i<MAX_STACKS; i++) {
        stack_init(&stacks[i]);
    }
    int num_stacks = 0;
    int line_num =0;
    while ((read = getline(&line, &len, fp)) != -1) {
         if(line[0] == '\n' || line[0] == '\r' ){
            break;
         }
        line_num++;
        if (num_stacks == 0) {
          num_stacks = (strlen(line) + 1) / 4;
        }
         
        for (int i = 1; i < strlen(line); i += 4) {
             if (isalpha(line[i])) {
                stack_push(&stacks[(i-1)/4], line[i]);
            }
        }
    }
    for(int i=0; i< num_stacks; i++){
        Stack temp;
        stack_init(&temp);
        while(!stack_empty(&stacks[i])){
            stack_push(&temp, stack_pop(&stacks[i]));
        }
        stacks[i] = temp;
    }
  
    int n, from, to;
    while ((read = getline(&line, &len, fp)) != -1) {
      if(strncmp(line, "move", 4) != 0){
          continue;
      }
        sscanf(line, "move %d from %d to %d", &n, &from, &to);
        from--;
        to--;
         for (int i = 0; i < n; i++) {
           stack_push(&stacks[to], stack_pop(&stacks[from]));
        }
    }

    for (int i = 0; i < num_stacks; i++) {
        printf("%c", stack_peek(&stacks[i]));
    }
    printf("\n");
    free(line);
    fclose(fp);
    return 0;
}
