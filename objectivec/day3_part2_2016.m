#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray *triangles = [NSMutableArray array];
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            NSArray *sides = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            sides = [sides filteredArrayUsingPredicate:[NSPredicate predicateWithFormat:@"SELF != ''"]];
            NSMutableArray *triangle = [NSMutableArray array];
            for (NSString *side in sides) {
                [triangle addObject:@(side.intValue)];
                if (triangle.count == 3) {
                    [triangles addObject:triangle];
                    triangle = [NSMutableArray array];
                }
            }
        }
        
        int possibleTriangles = 0;
        
        for (int i = 0; i < triangles.count; i += 3) {
            NSMutableArray *triangle1 = triangles[i];
            NSMutableArray *triangle2 = triangles[i+1];
            NSMutableArray *triangle3 = triangles[i+2];
            
            for (int j = 0; j < 3; j++) {
                int side1 = [triangle1[j] intValue];
                int side2 = [triangle2[j] intValue];
                int side3 = [triangle3[j] intValue];
                
                if (side1 + side2 > side3 && side1 + side3 > side2 && side2 + side3 > side1) {
                    possibleTriangles++;
                }
            }
        }
        
        printf("%d\n", possibleTriangles);
    }
    return 0;
}