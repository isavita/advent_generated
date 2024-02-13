#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *startingNumbers = [input componentsSeparatedByString:@","];
        
        NSMutableDictionary *memory = [NSMutableDictionary dictionary];
        int lastNumber = 0;
        
        for (int i = 0; i < startingNumbers.count; i++) {
            int number = [startingNumbers[i] intValue];
            memory[@(number)] = @(i + 1);
            lastNumber = number;
        }
        
        for (int i = startingNumbers.count; i < 30000000; i++) {
            int nextNumber = 0;
            if (memory[@(lastNumber)] != nil) {
                nextNumber = i - [memory[@(lastNumber)] intValue];
            }
            memory[@(lastNumber)] = @(i);
            lastNumber = nextNumber;
        }
        
        printf("The 30000000th number spoken is %d\n", lastNumber);
    }
    return 0;
}