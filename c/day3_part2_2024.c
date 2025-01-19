
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

int main() {
    FILE *fp;
    char *input = NULL;
    long input_len;
    long totalSum = 0;
    bool enabled = true;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    if (fseek(fp, 0, SEEK_END) != 0) {
        perror("Error seeking end of file");
        fclose(fp);
        return 1;
    }
    input_len = ftell(fp);
    if (input_len == -1) {
        perror("Error getting file size");
        fclose(fp);
        return 1;
    }
    if(fseek(fp,0,SEEK_SET)!=0)
    {
       perror("Error seeking start of file");
        fclose(fp);
        return 1;
    }

    input = (char *)malloc(input_len + 1);
    if (input == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    if (fread(input, 1, input_len, fp) != input_len) {
        perror("Error reading file");
        free(input);
        fclose(fp);
        return 1;
    }
    input[input_len] = '\0';
    fclose(fp);
    char *p = input;
     while (*p) {
        if (strncmp(p, "mul(", 4) == 0) {
            if (enabled) {
                p += 4;
                 int num1 = 0;
                while(isdigit(*p))
                {
                    num1= num1*10 + (*p-'0');
                     p++;
                }

                if(*p != ','){
                    goto next_instruction;
                }
                   p++;
                int num2 = 0;

                while(isdigit(*p))
                {
                    num2= num2*10 + (*p-'0');
                     p++;
                }
                 if(*p != ')')
                 {
                     goto next_instruction;
                 }

                totalSum += num1 * num2;
            }
            p++;
        } else if (strncmp(p, "do()", 4) == 0) {
            enabled = true;
            p += 4;
        } else if (strncmp(p, "don't()", 7) == 0) {
            enabled = false;
            p += 7;
        }else{
          p++;
        }
          next_instruction:;
    }
    printf("%ld\n", totalSum);
    free(input);

    return 0;
}
