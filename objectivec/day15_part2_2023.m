#import <Foundation/Foundation.h>

// Function to calculate the HASH value of a string
int hashValue(NSString *string) {
    int currentValue = 0;
    for (int i = 0; i < [string length]; i++) {
        unichar character = [string characterAtIndex:i];
        currentValue += character;
        currentValue *= 17;
        currentValue %= 256;
    }
    return currentValue;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        if (!fileContents) {
            NSLog(@"Error reading file");
            return 1;
        }

        // Split the input by commas to get individual steps
        NSArray *steps = [fileContents componentsSeparatedByString:@","];
        int sumOfHashes = 0;

        // Dictionary to store boxes, each box is an array of lenses
        NSMutableDictionary *boxes = [NSMutableDictionary dictionary];
        for (int i = 0; i < 256; i++) {
            boxes[@(i)] = [NSMutableArray array];
        }

        // Process each step
        for (NSString *step in steps) {
            NSString *trimmedStep = [step stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            sumOfHashes += hashValue(trimmedStep);

            // Determine label and operation
            NSString *label;
            NSString *operation;
            NSString *focalLength;
            if ([trimmedStep containsString:@"="]) {
                NSArray *components = [trimmedStep componentsSeparatedByString:@"="];
                label = components[0];
                operation = @"=";
                focalLength = components[1];
            } else {
                label = [trimmedStep substringToIndex:trimmedStep.length - 1];
                operation = @"-";
            }

            // Determine the box number using the HASH algorithm
            int boxNumber = hashValue(label);
            NSMutableArray *box = boxes[@(boxNumber)];

            if ([operation isEqualToString:@"="]) {
                BOOL found = NO;
                for (NSMutableDictionary *lens in box) {
                    if ([lens[@"label"] isEqualToString:label]) {
                        lens[@"focalLength"] = focalLength;
                        found = YES;
                        break;
                    }
                }
                if (!found) {
                    [box addObject:[NSMutableDictionary dictionaryWithObjectsAndKeys:label, @"label", focalLength, @"focalLength", nil]];
                }
            } else if ([operation isEqualToString:@"-"]) {
                NSMutableArray *newBox = [NSMutableArray array];
                for (NSMutableDictionary *lens in box) {
                    if (![lens[@"label"] isEqualToString:label]) {
                        [newBox addObject:lens];
                    }
                }
                boxes[@(boxNumber)] = newBox;
            }
        }

        // Calculate the focusing power
        int focusingPower = 0;
        for (int boxNumber = 0; boxNumber < 256; boxNumber++) {
            NSMutableArray *box = boxes[@(boxNumber)];
            for (int slot = 0; slot < [box count]; slot++) {
                NSDictionary *lens = box[slot];
                int focalLength = [lens[@"focalLength"] intValue];
                focusingPower += (boxNumber + 1) * (slot + 1) * focalLength;
            }
        }

        // Print the results
        NSLog(@"Sum of HASH values: %d", sumOfHashes);
        NSLog(@"Focusing power: %d", focusingPower);
    }
    return 0;
}