
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef enum {
    INT,
    LIST
} type_t;

typedef struct node {
    type_t type;
    union {
        int integer;
        struct node** list;
    };
    int list_size;
} node_t;

int sign(int n) {
    return (n > 0) - (n < 0);
}

node_t* parse_list(const char** s);

node_t* parse_value(const char** s) {
    while(isspace(**s)) (*s)++;
    if(**s == '[') {
        (*s)++;
        return parse_list(s);
    }
    int num = 0;
    while(isdigit(**s)) {
        num = num * 10 + (**s - '0');
        (*s)++;
    }
    node_t* node = malloc(sizeof(node_t));
    node->type = INT;
    node->integer = num;
    return node;
}


node_t* parse_list(const char** s) {
    node_t* node = malloc(sizeof(node_t));
    node->type = LIST;
    node->list = NULL;
    node->list_size = 0;
    while(**s != ']') {
        node_t* item = parse_value(s);
        node->list = realloc(node->list, (node->list_size + 1) * sizeof(node_t*));
        node->list[node->list_size++] = item;
        if(**s == ',') (*s)++;
    }
    (*s)++;
    return node;
}


void free_node(node_t* node) {
    if(node == NULL) return;
    if(node->type == LIST) {
        for(int i = 0; i < node->list_size; i++) {
            free_node(node->list[i]);
        }
        free(node->list);
    }
    free(node);
}

int compare(node_t* a, node_t* b) {
    if(a->type == INT && b->type == INT) {
        return sign(a->integer - b->integer);
    }
    if(a->type == INT) {
        node_t* list_a = malloc(sizeof(node_t));
        list_a->type = LIST;
        list_a->list = malloc(sizeof(node_t*));
        list_a->list[0] = a;
        list_a->list_size = 1;
        int res = compare(list_a, b);
        free(list_a->list);
        free(list_a);
        return res;
    }
    if(b->type == INT) {
        node_t* list_b = malloc(sizeof(node_t));
        list_b->type = LIST;
        list_b->list = malloc(sizeof(node_t*));
        list_b->list[0] = b;
        list_b->list_size = 1;
        int res = compare(a, list_b);
         free(list_b->list);
        free(list_b);
        return res;
    }
    int min_len = (a->list_size < b->list_size) ? a->list_size : b->list_size;
    for(int i = 0; i < min_len; i++) {
        int c = compare(a->list[i], b->list[i]);
        if(c != 0) return c;
    }
    return sign(a->list_size - b->list_size);
}


int main() {
    FILE* f = fopen("input.txt", "r");
    if(!f) {
        perror("Failed to open input.txt");
        return 1;
    }
    
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buffer = malloc(fsize + 1);
    fread(buffer, 1, fsize, f);
    fclose(f);
    buffer[fsize] = 0;

    int sum = 0;
    char* line = buffer;
    int pair_index = 1;
    while(*line) {
        char* first_start = line;
        while(*line && *line != '\n') line++;
        *line = 0;
        line++;
        char* second_start = line;
        while(*line && *line != '\n' ) line++;
        if(*line == '\n') *line = 0;
         line++;
        if (*first_start == 0) break;
         const char* first_ptr = first_start;
         const char* second_ptr = second_start;
        node_t* first = parse_value(&first_ptr);
        node_t* second = parse_value(&second_ptr);
        
        if(compare(first, second) == -1) {
            sum += pair_index;
        }
        
         free_node(first);
         free_node(second);

        pair_index++;
        while(*line == '\n' ) line++;
    }
    printf("%d\n", sum);
    free(buffer);
    return 0;
}
