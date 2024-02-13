
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSInteger numElves = [input integerValue];
        
        NSInteger result = 0;
        for (NSInteger i = 1; i <= numElves; i++) {
            result = (result + 2) % i;
        }
        
        printf("Elf %ld gets all the presents.\n", result + 1);
    }
    return 0;
}
