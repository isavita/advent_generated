
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_PATH_LEN 256
#define MAX_DIRS 1000
#define MAX_FILES 10000
#define TOTAL_SPACE 70000000
#define REQUIRED_SPACE 30000000

typedef struct {
    char path[MAX_PATH_LEN];
    int size;
} Dir;

typedef struct {
    char path[MAX_PATH_LEN];
    int size;
} File;

int compare_ints(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    Dir dirs[MAX_DIRS] = {0};
    File files[MAX_FILES] = {0};
    int dir_count = 0;
    int file_count = 0;
    char current_path[MAX_PATH_LEN] = "/";
    char line[512];

    while (fgets(line, sizeof(line), fp) != NULL) {
        char *token = strtok(line, " \n");
         if (token == NULL) continue;

        if (strcmp(token, "$") == 0) {
            token = strtok(NULL, " \n");
             if (token == NULL) continue;
            if (strcmp(token, "cd") == 0) {
                token = strtok(NULL, " \n");
                 if (token == NULL) continue;
                if (strcmp(token, "/") == 0) {
                    strcpy(current_path, "/");
                } else if (strcmp(token, "..") == 0) {
                   char* last_slash = strrchr(current_path, '/');
                    if (last_slash != current_path) {
                        *last_slash = '\0';
                    }else{
                         strcpy(current_path, "/");
                    }
                } else {
                    if (strcmp(current_path,"/") != 0){
                      strcat(current_path, "/");
                    }
                    strcat(current_path, token);
                }
                  bool dir_exists = false;
                    for(int i = 0; i < dir_count; i++) {
                        if (strcmp(dirs[i].path, current_path) == 0) {
                            dir_exists = true;
                            break;
                        }
                    }
                     if (!dir_exists) {
                            strcpy(dirs[dir_count].path, current_path);
                            dirs[dir_count].size = 0;
                            dir_count++;
                        }

            }
        } else if (strcmp(token, "dir") != 0) {
            int file_size = atoi(token);
            token = strtok(NULL, " \n");
             if(token == NULL) continue;
                
           
            char file_path[MAX_PATH_LEN];
            strcpy(file_path, current_path);
            if (strcmp(file_path, "/") != 0){
                strcat(file_path, "/");
            }
            strcat(file_path, token);

            strcpy(files[file_count].path, file_path);
            files[file_count].size = file_size;
             file_count++;

        }
    }
     fclose(fp);

    for (int i = 0; i < file_count; i++) {
        char *path = strdup(files[i].path);
       char* token = strtok(path, "/");
        char current_dir[MAX_PATH_LEN] = "/";
        while (token != NULL)
        {
            
             bool dir_exists = false;
            for(int j=0; j< dir_count; j++){
                 if (strcmp(dirs[j].path, current_dir) == 0)
                {
                   dirs[j].size += files[i].size;
                    dir_exists = true;
                   break;
                }
            }
             if (!dir_exists) {
                 
                 strcpy(dirs[dir_count].path, current_dir);
                 dirs[dir_count].size = files[i].size;
                 dir_count++;
             }
             if(strcmp(current_dir, "/") != 0){
                  strcat(current_dir, "/");
             }
             
             
             strcat(current_dir, token);
            token = strtok(NULL, "/");
         }
          free(path);
    }
    int sorted_sizes[MAX_DIRS];
    for (int i = 0; i < dir_count; i++) {
        sorted_sizes[i] = dirs[i].size;
    }

    qsort(sorted_sizes, dir_count, sizeof(int), compare_ints);

    int total_used = 0;
     for (int i = 0; i < dir_count; i++) {
         if (strcmp(dirs[i].path, "/") == 0){
             total_used = dirs[i].size;
             break;
         }
     }
    int available_space = TOTAL_SPACE - total_used;
    int needed_space = REQUIRED_SPACE - available_space;
    
    int result = -1;
    for(int i=0; i< dir_count; i++){
        if (sorted_sizes[i] >= needed_space){
             result = sorted_sizes[i];
            break;
        }
    }

    printf("%d\n", result);

    return 0;
}
