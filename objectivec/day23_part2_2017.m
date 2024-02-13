#import <Foundation/Foundation.h>

BOOL isPrime(int n) {
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return NO;
        }
    }
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (file == NULL) {
            printf("Error opening file\n");
            return 1;
        }
        
        int b = 57*100 + 100000;
        int c = b + 17000;
        int h = 0;
        
        for (int x = b; x <= c; x += 17) {
            if (!isPrime(x)) {
                h++;
            }
        }
        
        printf("%d\n", h);
        
        fclose(file);
    }
    return 0;
}