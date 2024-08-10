#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *data = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSInteger target = [[data stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]] integerValue] / 11;
        NSMutableArray<NSNumber *> *houses = [NSMutableArray arrayWithCapacity:target + 1];
        
        for (NSInteger i = 0; i <= target; i++) {
            [houses addObject:@(0)];
        }
        
        for (NSInteger elf = 1; elf <= target; elf++) {
            for (NSInteger house = elf; house <= elf * 50 && house <= target; house += elf) {
                houses[house] = @([houses[house] integerValue] + elf);
            }
        }
        
        for (NSInteger houseNumber = 1; houseNumber <= target; houseNumber++) {
            if ([houses[houseNumber] integerValue] >= target) {
                NSLog(@"%ld", (long)houseNumber);
                break;
            }
        }
    }
    return 0;
}