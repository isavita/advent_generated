
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool supportsSSL(char *ip) {
    char *bracketContents[100];
    int bracketCount = 0;
    char *temp = strdup(ip);
    char *token = temp;
    int len = strlen(ip);
    while (*token) {
        if (*token == '[') {
            char *end = strchr(token + 1, ']');
            if (end) {
                int contentLen = end - token - 1;
                bracketContents[bracketCount] = (char *)malloc(contentLen + 1);
                strncpy(bracketContents[bracketCount], token + 1, contentLen);
                bracketContents[bracketCount][contentLen] = '\0';
                bracketCount++;
                for(int i = 0; i <= contentLen+1; i++)
                    token++;

            }else{
                token++;
            }
            
        }else{
            token++;
        }
       
    }
    
    char *modifiedIp = (char*)malloc(len + 1);
    int modIdx = 0;
    for(int i = 0; i < len; i++){
        if (ip[i] == '['){
             while(i < len && ip[i] != ']')
                 i++;
            
        }
        else{
           modifiedIp[modIdx++] = ip[i];
        }
       
    }
    modifiedIp[modIdx] = '\0';
    
    for (int i = 0; modifiedIp[i] && modifiedIp[i+2]; i++) {
        if (modifiedIp[i] != modifiedIp[i+1] && modifiedIp[i] == modifiedIp[i+2]) {
            char bab[4] = {modifiedIp[i+1], modifiedIp[i], modifiedIp[i+1], '\0'};
            for (int j = 0; j < bracketCount; j++) {
                if (strstr(bracketContents[j], bab) != NULL) {
                    free(temp);
                    free(modifiedIp);
                    for(int k = 0; k < bracketCount; k++)
                        free(bracketContents[k]);
                    return true;
                }
            }
        }
    }
    free(temp);
     free(modifiedIp);
     for(int k = 0; k < bracketCount; k++)
        free(bracketContents[k]);
    return false;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int sslCount = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        if (line[read - 1] == '\n') {
            line[read - 1] = '\0';
        }
        if (supportsSSL(line)) {
            sslCount++;
        }
    }

    free(line);
    fclose(file);
    printf("%d\n", sslCount);
    return 0;
}
