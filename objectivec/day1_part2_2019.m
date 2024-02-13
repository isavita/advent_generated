#import <Foundation/Foundation.h>

int calculateFuel(int mass) {
    return mass / 3 - 2;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *modules = [input componentsSeparatedByString:@"\n"];
        
        int totalFuel = 0;
        for (NSString *module in modules) {
            int mass = [module intValue];
            int fuel = calculateFuel(mass);
            totalFuel += fuel;
            
            int additionalFuel = calculateFuel(fuel);
            while (additionalFuel > 0) {
                totalFuel += additionalFuel;
                additionalFuel = calculateFuel(additionalFuel);
            }
        }
        
        printf("%d\n", totalFuel);
    }
    return 0;
}