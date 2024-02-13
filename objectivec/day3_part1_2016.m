#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int possibleTriangles = 0;
        
        for (NSString *line in lines) {
            NSArray *sides = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            sides = [sides filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF != ''"]];
            
            int side1 = [sides[0] intValue];
            int side2 = [sides[1] intValue];
            int side3 = [sides[2] intValue];
            
            if (side1 + side2 > side3 && side1 + side3 > side2 && side2 + side3 > side1) {
                possibleTriangles++;
            }
        }
        
        printf("%d\n", possibleTriangles);
    }
    return 0;
}