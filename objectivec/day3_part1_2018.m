#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *claims = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *fabric = [NSMutableDictionary dictionary];
        int overlappingInches = 0;
        
        for (NSString *claim in claims) {
            NSArray *components = [claim componentsSeparatedByString:@" "];
            NSString *position = components[2];
            NSString *size = components[3];
            
            NSArray *posComponents = [position componentsSeparatedByString:@","];
            int left = [posComponents[0] intValue];
            int top = [posComponents[1] intValue];
            
            NSArray *sizeComponents = [size componentsSeparatedByString:@"x"];
            int width = [sizeComponents[0] intValue];
            int height = [sizeComponents[1] intValue];
            
            for (int i = left; i < left + width; i++) {
                for (int j = top; j < top + height; j++) {
                    NSString *key = [NSString stringWithFormat:@"%d,%d", i, j];
                    if (fabric[key]) {
                        if ([fabric[key] intValue] == 1) {
                            overlappingInches++;
                        }
                        fabric[key] = @2;
                    } else {
                        fabric[key] = @1;
                    }
                }
            }
        }
        
        printf("%d\n", overlappingInches);
    }
    return 0;
}