
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NAME_LEN 256
#define MAX_DIRS 1000
#define MAX_FILES 1000

typedef struct {
    int size;
} File;

typedef struct Directory {
    char name[MAX_NAME_LEN];
    File files[MAX_FILES];
    int fileCount;
    struct Directory* directories[MAX_DIRS];
    int dirCount;
} Directory;

Directory* newDirectory(const char* name) {
    Directory* dir = (Directory*)malloc(sizeof(Directory));
    strcpy(dir->name, name);
    dir->fileCount = 0;
    dir->dirCount = 0;
    return dir;
}

int totalSize(Directory* dir) {
    int size = 0;
    for (int i = 0; i < dir->fileCount; i++) {
        size += dir->files[i].size;
    }
    for (int i = 0; i < dir->dirCount; i++) {
        size += totalSize(dir->directories[i]);
    }
    return size;
}

void calculateSizes(Directory* dir, int* sumSizes) {
    int dirSize = totalSize(dir);
    if (dirSize <= 100000) {
        *sumSizes += dirSize;
    }
    for (int i = 0; i < dir->dirCount; i++) {
        calculateSizes(dir->directories[i], sumSizes);
    }
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    Directory* root = newDirectory("/");
    Directory* currentDir = root;
    Directory* directoryStack[MAX_DIRS];
    int stackSize = 0;
    directoryStack[stackSize++] = root;

    char line[256];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline character
        if (strncmp(line, "$ cd", 4) == 0) {
            char* path = line + 4;
            while (*path == ' ') path++;
            if (strcmp(path, "/") == 0) {
                currentDir = root;
                stackSize = 1;
            } else if (strcmp(path, "..") == 0) {
                stackSize--;
                currentDir = directoryStack[stackSize - 1];
            } else {
                int found = 0;
                for (int i = 0; i < currentDir->dirCount; i++) {
                    if (strcmp(currentDir->directories[i]->name, path) == 0) {
                        currentDir = currentDir->directories[i];
                        directoryStack[stackSize++] = currentDir;
                        found = 1;
                        break;
                    }
                }
                if (!found) {
                    Directory* newDir = newDirectory(path);
                    currentDir->directories[currentDir->dirCount++] = newDir;
                    currentDir = newDir;
                    directoryStack[stackSize++] = currentDir;
                }
            }
        } else if (strncmp(line, "dir", 3) == 0) {
            char* dirName = line + 4;
            while (*dirName == ' ') dirName++;
            Directory* newDir = newDirectory(dirName);
            currentDir->directories[currentDir->dirCount++] = newDir;
        } else {
            char* sizeStr = strtok(line, " ");
            char* fileName = strtok(NULL, " ");
            if (sizeStr && fileName) {
                int size = atoi(sizeStr);
                File newFile = {size};
                currentDir->files[currentDir->fileCount++] = newFile;
            }
        }
    }
    fclose(file);

    int sumSizes = 0;
    calculateSizes(root, &sumSizes);

    printf("%d\n", sumSizes);

    return 0;
}
