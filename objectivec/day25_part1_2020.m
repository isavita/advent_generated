#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (file == NULL) {
            printf("Error opening file\n");
            return 1;
        }
        
        long cardPublicKey, doorPublicKey;
        fscanf(file, "%ld\n%ld", &cardPublicKey, &doorPublicKey);
        fclose(file);
        
        long subjectNumber = 7;
        long value = 1;
        long loopSize = 0;
        
        while (value != cardPublicKey) {
            value *= subjectNumber;
            value %= 20201227;
            loopSize++;
        }
        
        long encryptionKey = 1;
        for (int i = 0; i < loopSize; i++) {
            encryptionKey *= doorPublicKey;
            encryptionKey %= 20201227;
        }
        
        printf("Encryption key: %ld\n", encryptionKey);
    }
    return 0;
}