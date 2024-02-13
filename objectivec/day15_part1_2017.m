#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (file == NULL) {
            printf("Error opening the file\n");
            return 1;
        }
        
        unsigned long long genA = 0;
        unsigned long long genB = 0;
        unsigned long long factorA = 16807;
        unsigned long long factorB = 48271;
        unsigned long long divisor = 2147483647;
        int matchCount = 0;
        
        for (int i = 0; i < 40000000; i++) {
            fscanf(file, "%llu %llu", &genA, &genB);
            genA = (genA * factorA) % divisor;
            genB = (genB * factorB) % divisor;
            if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
                matchCount++;
            }
        }
        
        fclose(file);
        
        printf("%d\n", matchCount);
    }
    return 0;
}