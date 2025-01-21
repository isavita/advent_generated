
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 1024
#define MAX_NAME_LENGTH 16
#define MAX_HOLDS 16

typedef struct {
    int weight;
    char holds[MAX_HOLDS][MAX_NAME_LENGTH];
    int num_holds;
} Program;

typedef struct Node {
    char name[MAX_NAME_LENGTH];
    Program program;
    struct Node *next;
} Node;

Node *head = NULL;

Node *createNode(const char *name, int weight) {
    Node *newNode = (Node *)malloc(sizeof(Node));
    strcpy(newNode->name, name);
    newNode->program.weight = weight;
    newNode->program.num_holds = 0;
    newNode->next = NULL;
    return newNode;
}

void addNode(const char *name, int weight) {
    Node *newNode = createNode(name, weight);
    newNode->next = head;
    head = newNode;
}

Node *findNode(const char *name) {
    Node *current = head;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

int dfs(const char *name, int *balanced) {
    Node *node = findNode(name);
    Program program = node->program;
    int totalWeight = program.weight;

    int weights[MAX_HOLDS];
    int counts[MAX_HOLDS];
    memset(counts, 0, sizeof(counts));
    int num_unique_weights = 0;

    for (int i = 0; i < program.num_holds; i++) {
        int child_balanced = 1;
        int weight = dfs(program.holds[i], &child_balanced);
        if (!child_balanced) {
            *balanced = 0;
            return 0;
        }
        totalWeight += weight;

        int found = 0;
        for (int j = 0; j < num_unique_weights; j++) {
          if(weights[j] == weight){
            counts[j]++;
            found = 1;
            break;
          }
        }
        if(!found){
          weights[num_unique_weights] = weight;
          counts[num_unique_weights]++;
          num_unique_weights++;
        }
    }
    
    if(num_unique_weights > 1){
        int w1 = -1, c1 = 0;
        int w2 = -1, c2 = 0;
        for(int i=0; i< num_unique_weights; ++i){
            if(w1 == -1){
                w1 = weights[i];
                c1 = counts[i];
            } else if (w2 == -1) {
                w2 = weights[i];
                c2 = counts[i];
            } else if (weights[i] != w1 && weights[i] != w2){
                
            }
        }
        if(w1 != -1 && w2 != -1){
            if(c1 < c2) {
                for (int i = 0; i < program.num_holds; i++) {
                  int child_balanced = 1;
                    if (dfs(program.holds[i], &child_balanced) == w1) {
                        printf("%d\n", findNode(program.holds[i])->program.weight + (w2 - w1));
                        *balanced = 0;
                        return 0;
                    }
                }
            } else {
              for (int i = 0; i < program.num_holds; i++) {
                int child_balanced = 1;
                    if (dfs(program.holds[i], &child_balanced) == w2) {
                        printf("%d\n", findNode(program.holds[i])->program.weight + (w1 - w2));
                        *balanced = 0;
                        return 0;
                    }
                }
            }
        }
    }
    
    *balanced = 1;
    return totalWeight;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        char name[MAX_NAME_LENGTH];
        int weight;
        char *token;
        
        token = strtok(line, " ");
        strcpy(name,token);

        token = strtok(NULL, " ");
        sscanf(token, "(%d)", &weight);

        addNode(name, weight);
    }
    
    rewind(file);
    
    while (fgets(line, sizeof(line), file)) {
      char name[MAX_NAME_LENGTH];
        char *token;
        
        token = strtok(line, " ");
        strcpy(name,token);

        Node* curr = findNode(name);
        token = strtok(NULL, " ");
        token = strtok(NULL, " ");

        while(token != NULL){
          if(token[0] == '-'){
            token = strtok(NULL, " ");
            continue;
          }
          char hold_name[MAX_NAME_LENGTH];
          int len = strlen(token);
          if(token[len-1] == '\n' || token[len-1] == ','){
            strncpy(hold_name, token, len-1);
            hold_name[len-1] = '\0';
            strcpy(curr->program.holds[curr->program.num_holds], hold_name);
            curr->program.num_holds++;
          } else {
            strcpy(curr->program.holds[curr->program.num_holds], token);
            curr->program.num_holds++;
          }
          
          token = strtok(NULL, " ");
        }
    }

    fclose(file);

    int balanced;
    dfs("dtacyn", &balanced);

    return 0;
}
