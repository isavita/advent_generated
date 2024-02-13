#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        NSMutableArray *cups = [NSMutableArray array];
        for (int i = 0; i < input.length; i++) {
            [cups addObject:[NSNumber numberWithInt:[[input substringWithRange:NSMakeRange(i, 1)] intValue]]];
        }
        
        int currentCupIndex = 0;
        for (int i = 0; i < 100; i++) {
            int currentCup = [[cups objectAtIndex:currentCupIndex] intValue];
            NSMutableArray *pickedUp = [NSMutableArray array];
            for (int j = 1; j <= 3; j++) {
                int index = (currentCupIndex + j) % cups.count;
                [pickedUp addObject:[cups objectAtIndex:index]];
            }
            for (NSNumber *cup in pickedUp) {
                [cups removeObject:cup];
            }
            
            int destinationCup = currentCup - 1;
            while (![cups containsObject:[NSNumber numberWithInt:destinationCup]]) {
                destinationCup--;
                if (destinationCup < 1) {
                    destinationCup = 9;
                }
            }
            
            int destinationIndex = (int)[cups indexOfObject:[NSNumber numberWithInt:destinationCup]] + 1;
            for (NSNumber *cup in pickedUp) {
                [cups insertObject:cup atIndex:destinationIndex];
                destinationIndex++;
            }
            
            currentCupIndex = (int)[cups indexOfObject:[NSNumber numberWithInt:currentCup]] + 1;
            if (currentCupIndex == cups.count) {
                currentCupIndex = 0;
            }
        }
        
        int indexOfOne = (int)[cups indexOfObject:[NSNumber numberWithInt:1]];
        NSMutableString *result = [NSMutableString string];
        for (int i = 1; i < cups.count; i++) {
            int index = (indexOfOne + i) % cups.count;
            [result appendFormat:@"%d", [[cups objectAtIndex:index] intValue]];
        }
        
        printf("%s\n", [result UTF8String]);
    }
    return 0;
}