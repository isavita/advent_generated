
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

long long decompress(char *input, int start, int end);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    if ((read = getline(&line, &len, file)) != -1) {
        long long decompressedLength = decompress(line, 0, strlen(line));
        printf("%lld\n", decompressedLength);
    }
     if(line) free(line);
    fclose(file);
    return 0;
}

long long decompress(char *input, int start, int end) {
    long long length = 0;
    for (int i = start; i < end;) {
        if (input[i] == '(') {
            int charCount = 0;
            int repeatCount = 0;
            int j = i + 1;
            while(isdigit(input[j])) {
                charCount = charCount * 10 + (input[j] - '0');
                j++;
            }
            j++;
             while(isdigit(input[j])) {
                repeatCount = repeatCount * 10 + (input[j] - '0');
                j++;
            }
           
            int nextIndex = j + 1;
            length += repeatCount * decompress(input, nextIndex, nextIndex + charCount);
            i = nextIndex + charCount;
        } else {
            length++;
            i++;
        }
    }
    return length;
}
