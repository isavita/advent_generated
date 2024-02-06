#include <stdio.h>
#include <stdlib.h>

int main() {
    int arr[200];
    FILE *fp;
    fp = fopen("input.txt", "r");

    if(fp == NULL) {
        printf("File not found");
        exit(1);
    }

    int num;
    int index = 0;

    while(fscanf(fp, "%d", &num) != EOF) {
        arr[index] = num;
        index++;
    }

    fclose(fp);

    for(int i = 0; i < index; i++) {
        for(int j = i+1; j < index; j++) {
            if(arr[i] + arr[j] == 2020) {
                printf("%d", arr[i] * arr[j]);
                break;
            }
        }
    }

    return 0;
}