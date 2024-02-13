#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *modules = [input componentsSeparatedByString:@"\n"];
        
        int totalFuel = 0;
        for (NSString *module in modules) {
            int mass = [module intValue];
            int fuel = (mass / 3) - 2;
            totalFuel += fuel;
        }
        
        printf("Total fuel required: %d\n", totalFuel);
    }
    return 0;
}