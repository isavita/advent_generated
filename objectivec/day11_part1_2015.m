#import <Foundation/Foundation.h>

// Function to increment a character
char incrementChar(char c) {
    if (c == 'z') return 'a';
    return c + 1;
}

// Function to check if a string contains at least one increasing straight of at least three letters
BOOL hasIncreasingStraight(NSString *str) {
    for (NSInteger i = 0; i < str.length - 2; i++) {
        if ([str characterAtIndex:i] == [str characterAtIndex:i + 1] - 1 &&
            [str characterAtIndex:i + 1] == [str characterAtIndex:i + 2] - 1) {
            return YES;
        }
    }
    return NO;
}

// Function to check if a string contains at least two different, non-overlapping pairs of letters
BOOL hasTwoPairs(NSString *str) {
    NSInteger pairs = 0;
    for (NSInteger i = 0; i < str.length - 1; i++) {
        if ([str characterAtIndex:i] == [str characterAtIndex:i + 1]) {
            pairs++;
            i++; // Skip the next character
        }
    }
    return pairs >= 2;
}

// Function to check if a string contains any of the forbidden letters
BOOL hasForbiddenLetters(NSString *str) {
    for (NSInteger i = 0; i < str.length; i++) {
        char c = [str characterAtIndex:i];
        if (c == 'i' || c == 'l' || c == 'o') {
            return YES;
        }
    }
    return NO;
}

// Function to increment a string
NSString *incrementString(NSString *str) {
    NSMutableString *result = [NSMutableString stringWithString:str];
    for (NSInteger i = result.length - 1; i >= 0; i--) {
        char c = [result characterAtIndex:i];
        if (c == 'z') {
            [result replaceCharactersInRange:NSMakeRange(i, 1) withString:@"a"];
        } else {
            [result replaceCharactersInRange:NSMakeRange(i, 1) withString:[NSString stringWithFormat:@"%c", incrementChar(c)]];
            return result;
        }
    }
    return result;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        
        // Trim whitespace and convert to lowercase
        input = [input stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        input = [input lowercaseString];
        
        // Find the next valid password
        while (YES) {
            input = incrementString(input);
            if (!hasForbiddenLetters(input) && hasIncreasingStraight(input) && hasTwoPairs(input)) {
                break;
            }
        }
        
        // Print the result
        NSLog(@"%@", input);
    }
    return 0;
}