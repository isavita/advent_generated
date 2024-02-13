#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int totalPaper = 0;
        
        for (NSString *line in lines) {
            NSArray *dimensions = [line componentsSeparatedByString:@"x"];
            int l = [dimensions[0] intValue];
            int w = [dimensions[1] intValue];
            int h = [dimensions[2] intValue];
            
            int side1 = l * w;
            int side2 = w * h;
            int side3 = h * l;
            
            int extra = MIN(MIN(side1, side2), side3);
            
            int paper = 2*l*w + 2*w*h + 2*h*l + extra;
            
            totalPaper += paper;
        }
        
        printf("%d\n", totalPaper);
    }
    return 0;
}