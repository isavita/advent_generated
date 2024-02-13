#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *programsAbove = [NSMutableDictionary dictionary];
        NSMutableSet *allPrograms = [NSMutableSet set];
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" -> "];
            NSString *programInfo = components[0];
            NSArray *programParts = [programInfo componentsSeparatedByString:@" "];
            NSString *programName = programParts[0];
            [allPrograms addObject:programName];
            
            if (components.count > 1) {
                NSString *abovePrograms = components[1];
                NSArray *aboveProgramsList = [abovePrograms componentsSeparatedByString:@", "];
                for (NSString *aboveProgram in aboveProgramsList) {
                    [allPrograms addObject:aboveProgram];
                    programsAbove[aboveProgram] = programName;
                }
            }
        }
        
        NSString *bottomProgram = @"";
        for (NSString *program in allPrograms) {
            if (![programsAbove objectForKey:program]) {
                bottomProgram = program;
                break;
            }
        }
        
        printf("%s\n", [bottomProgram UTF8String]);
    }
    return 0;
}