
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 1024
#define MAX_NAME_LEN 8
typedef struct Node {
    char name[MAX_NAME_LEN];
    struct Node* children[MAX_NODES];
    int numChildren;
    struct Node* parent;
} Node;


Node* nodes[MAX_NODES];
int nodeCount=0;


int hash(const char* str){
    int hash = 5381;
    int c;
    while(c = *str++){
       hash = ((hash << 5) + hash) + c;
    }
    return hash % MAX_NODES;
}


Node* findOrCreateNode(const char* name) {
  int h = hash(name);
    
  for(int i = 0; i < nodeCount; ++i){
      if(nodes[i] != NULL && strcmp(nodes[i]->name, name) == 0){
          return nodes[i];
      }
    }
    
  Node* node = (Node*)malloc(sizeof(Node));
    if(node == NULL){
        perror("malloc failed");
        exit(EXIT_FAILURE);
    }
  strcpy(node->name,name);
  node->numChildren = 0;
  node->parent = NULL;
  nodes[nodeCount++] = node;

  return node;
}

void buildOrbitMap(FILE* file) {
  char line[20];
  while (fgets(line, sizeof(line), file) != NULL) {
      char* centerName = strtok(line, ")");
      char* orbiterName = strtok(NULL, "\n");
      
      if (centerName == NULL || orbiterName == NULL) {
            continue;
      }
      
      Node* center = findOrCreateNode(centerName);
      Node* orbiter = findOrCreateNode(orbiterName);
      
      center->children[center->numChildren++] = orbiter;
      orbiter->parent = center;
  }
}

Node** pathToRoot(Node* node, int* pathLen) {
  Node** path = (Node**)malloc(sizeof(Node*) * MAX_NODES);
  if(path == NULL){
      perror("malloc failed");
      exit(EXIT_FAILURE);
  }

  *pathLen = 0;
  while (node != NULL) {
    path[(*pathLen)++] = node;
    node = node->parent;
  }
  return path;
}

void freePath(Node** path){
    free(path);
}

void freeNodes(){
    for(int i=0; i<nodeCount; i++){
       free(nodes[i]);
    }
}

int findCommonAncestor(Node* node1, Node* node2) {
  int pathLen1, pathLen2;
  Node** path1 = pathToRoot(node1, &pathLen1);
  Node** path2 = pathToRoot(node2, &pathLen2);
  

  int i = pathLen1 - 1;
  int j = pathLen2 - 1;

  while (i >= 0 && j >= 0 && path1[i] == path2[j]) {
    i--;
    j--;
  }
  
  freePath(path1);
  freePath(path2);

  return i + 1 + j + 1;
}

int main() {
  FILE* file = fopen("input.txt", "r");
  if (file == NULL) {
    perror("Error opening file");
    return 1;
  }

  buildOrbitMap(file);
  fclose(file);

  Node* you = NULL;
  Node* san = NULL;
    for(int i = 0; i < nodeCount; ++i){
        if (nodes[i] != NULL && strcmp(nodes[i]->name, "YOU") == 0) {
             you = nodes[i];
        }
        if (nodes[i] != NULL && strcmp(nodes[i]->name, "SAN") == 0) {
            san = nodes[i];
        }
    }
    
    
   if (you == NULL || san == NULL || you->parent == NULL || san->parent == NULL) {
        printf("YOU or SAN node or parents not found\n");
        return 1;
    }


    int transfers = findCommonAncestor(you->parent, san->parent);

    printf("%d\n", transfers);
    
    freeNodes();

  return 0;
}
