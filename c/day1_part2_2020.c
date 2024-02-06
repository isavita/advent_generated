#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int arr[200];
    int i = 0;
    
    while (fscanf(fp, "%d", &arr[i]) != EOF) {
        i++;
    }
    
    for (int j = 0; j < i; j++) {
        for (int k = j + 1; k < i; k++) {
            if (arr[j] + arr[k] == 2020) {
                printf("%d\n", arr[j] * arr[k]);
            }
            for (int l = k + 1; l < i; l++) {
                if (arr[j] + arr[k] + arr[l] == 2020) {
                    printf("%d\n", arr[j] * arr[k] * arr[l]);
                }
            }
        }
    }
    
    fclose(fp);
    
    return 0;
}