#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *orbitList = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *orbitMap = [NSMutableDictionary dictionary];
        
        for (NSString *orbit in orbitList) {
            NSArray *objects = [orbit componentsSeparatedByString:@")"];
            NSString *center = objects[0];
            NSString *orbiter = objects[1];
            orbitMap[orbiter] = center;
        }
        
        int totalOrbits = 0;
        
        for (NSString *object in orbitMap) {
            NSString *currentObject = object;
            while (orbitMap[currentObject]) {
                totalOrbits++;
                currentObject = orbitMap[currentObject];
            }
        }
        
        printf("%d\n", totalOrbits);
    }
    return 0;
}