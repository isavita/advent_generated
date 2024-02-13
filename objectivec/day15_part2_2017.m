#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSInteger genAStart = [lines[0] integerValue];
        NSInteger genBStart = [lines[1] integerValue];
        
        NSInteger genAFactor = 16807;
        NSInteger genBFactor = 48271;
        NSInteger modulus = 2147483647;
        
        NSInteger genA = genAStart;
        NSInteger genB = genBStart;
        NSInteger matches = 0;
        
        for (int i = 0; i < 5000000; i++) {
            // Generate next value for A that is a multiple of 4
            do {
                genA = (genA * genAFactor) % modulus;
            } while (genA % 4 != 0);
            
            // Generate next value for B that is a multiple of 8
            do {
                genB = (genB * genBFactor) % modulus;
            } while (genB % 8 != 0);
            
            if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
                matches++;
            }
        }
        
        printf("%ld\n", (long)matches);
    }
    
    return 0;
}