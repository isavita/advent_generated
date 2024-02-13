#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *groups = [input componentsSeparatedByString:@"\n\n"];
        
        int sumAnyone = 0;
        int sumEveryone = 0;
        
        for (NSString *group in groups) {
            NSCountedSet *set = [NSCountedSet new];
            NSArray *people = [group componentsSeparatedByString:@"\n"];
            int groupSize = people.count;
            
            for (NSString *person in people) {
                for (int i = 0; i < person.length; i++) {
                    NSString *answer = [person substringWithRange:NSMakeRange(i, 1)];
                    [set addObject:answer];
                }
            }
            
            for (NSString *question in set) {
                if ([set countForObject:question] == groupSize) {
                    sumEveryone++;
                }
            }
            
            sumAnyone += set.count;
        }
        
        printf("Part One: %d\n", sumAnyone);
        printf("Part Two: %d\n", sumEveryone);
    }
    return 0;
}