
#include <stdio.h>
#include <stdlib.h>

int sum_metadata(int* nums, int* index) {
    int num_child = nums[(*index)++];
    int num_metadata = nums[(*index)++];
    int sum = 0;
    
    for (int i = 0; i < num_child; i++) {
        sum += sum_metadata(nums, index);
    }
    
    for (int i = 0; i < num_metadata; i++) {
        sum += nums[(*index)++];
    }
    
    return sum;
}

int node_value(int* nums, int* index) {
    int num_child = nums[(*index)++];
    int num_metadata = nums[(*index)++];
    int value = 0;
    int* child_values = (int*)malloc(num_child * sizeof(int));
    
    for (int i = 0; i < num_child; i++) {
        child_values[i] = node_value(nums, index);
    }
    
    for (int i = 0; i < num_metadata; i++) {
        int metadata = nums[(*index)++];
        
        if (num_child == 0) {
            value += metadata;
        } else if (metadata > 0 && metadata <= num_child) {
            value += child_values[metadata - 1];
        }
    }
    
    free(child_values);
    
    return value;
}

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    
    int num;
    int count = 0;
    while (fscanf(fp, "%d", &num) != EOF) {
        count++;
    }
    
    int *nums = (int*)malloc(count * sizeof(int));
    
    fseek(fp, 0, SEEK_SET);
    
    for (int i = 0; i < count; i++) {
        fscanf(fp, "%d", &nums[i]);
    }
    
    fclose(fp);
    
    int index = 0;
    
    int sum = sum_metadata(nums, &index);
    printf("%d\n", sum);
    
    index = 0;
    
    int value = node_value(nums, &index);
    printf("%d\n", value);
    
    free(nums);
    
    return 0;
}
