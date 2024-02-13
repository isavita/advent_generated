#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *instructions = [input componentsSeparatedByString:@"\n"];
        
        NSDictionary *keypad = @{
            @"1": @{@"D": @"3"},
            @"2": @{@"R": @"3", @"D": @"6"},
            @"3": @{@"U": @"1", @"R": @"4", @"D": @"7", @"L": @"2"},
            @"4": @{@"L": @"3", @"D": @"8"},
            @"5": @{@"R": @"6"},
            @"6": @{@"U": @"2", @"R": @"7", @"D": @"A", @"L": @"5"},
            @"7": @{@"U": @"3", @"R": @"8", @"D": @"B", @"L": @"6"},
            @"8": @{@"U": @"4", @"R": @"9", @"D": @"C", @"L": @"7"},
            @"9": @{@"L": @"8"},
            @"A": @{@"U": @"6", @"R": @"B"},
            @"B": @{@"U": @"7", @"R": @"C", @"D": @"D", @"L": @"A"},
            @"C": @{@"U": @"8", @"L": @"B"},
            @"D": @{@"U": @"B"}
        };
        
        NSString *position = @"5";
        NSMutableString *code = [NSMutableString string];
        
        for (NSString *instruction in instructions) {
            for (int i = 0; i < instruction.length; i++) {
                unichar move = [instruction characterAtIndex:i];
                NSString *nextPos = keypad[position][[NSString stringWithFormat:@"%C", move]];
                if (nextPos) {
                    position = nextPos;
                }
            }
            [code appendString:position];
        }
        
        printf("%s\n", [code UTF8String]);
    }
    return 0;
}