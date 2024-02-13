#import <Foundation/Foundation.h>

int main() {
    NSError *error = nil;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"%@", [error localizedDescription]);
        return 1;
    }
    
    NSString *trimmedInput = [input stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    int target = [trimmedInput intValue] / 10;
    
    NSMutableArray<NSNumber *> *houses = [NSMutableArray arrayWithCapacity:target + 1];
    for (int i = 0; i <= target; i++) {
        [houses addObject:@0];
    }
    
    for (int elf = 1; elf <= target; elf++) {
        for (int house = elf; house <= target; house += elf) {
            int currentPresents = [houses[house] intValue];
            houses[house] = @(currentPresents + elf);
        }
    }
    
    for (int i = 0; i < houses.count; i++) {
        if ([houses[i] intValue] >= target) {
            printf("%d\n", i);
            break;
        }
    }
    
    return 0;
}