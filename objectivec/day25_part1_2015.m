#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        long long row = 2947;
        long long col = 3029;
        long long code = 20151125;
        
        long long diag = row + col - 1;
        long long pos = diag * (diag - 1) / 2 + col;
        
        for (long long i = 2; i <= pos; i++) {
            code = (code * 252533) % 33554393;
        }
        
        printf("Code for row %lld, column %lld: %lld\n", row, col, code);
    }
    return 0;
}