#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *numbers = [fileContents componentsSeparatedByString:@"\n"];
        
        NSMutableArray *preamble = [NSMutableArray new];
        int invalidNumber = 0;
        
        for (int i = 25; i < numbers.count; i++) {
            BOOL found = NO;
            int currentNumber = [numbers[i] intValue];
            
            for (int j = i - 25; j < i; j++) {
                int num1 = [numbers[j] intValue];
                
                for (int k = j + 1; k < i; k++) {
                    int num2 = [numbers[k] intValue];
                    
                    if (num1 + num2 == currentNumber) {
                        found = YES;
                        break;
                    }
                }
                
                if (found) {
                    break;
                }
            }
            
            if (!found) {
                invalidNumber = currentNumber;
                break;
            }
        }
        
        printf("Part One: %d\n", invalidNumber);
        
        int encryptionWeakness = 0;
        
        for (int i = 0; i < numbers.count; i++) {
            int sum = 0;
            int smallest = INT_MAX;
            int largest = INT_MIN;
            
            for (int j = i; j < numbers.count; j++) {
                int num = [numbers[j] intValue];
                sum += num;
                smallest = MIN(smallest, num);
                largest = MAX(largest, num);
                
                if (sum == invalidNumber) {
                    encryptionWeakness = smallest + largest;
                    break;
                } else if (sum > invalidNumber) {
                    break;
                }
            }
            
            if (encryptionWeakness != 0) {
                break;
            }
        }
        
        printf("Part Two: %d\n", encryptionWeakness);
    }
    return 0;
}