
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        
        NSMutableDictionary *houses = [NSMutableDictionary dictionary];
        NSPoint santa = NSMakePoint(0, 0);
        NSPoint roboSanta = NSMakePoint(0, 0);
        
        houses[[NSValue valueWithPoint:santa]] = @1;
        houses[[NSValue valueWithPoint:roboSanta]] = @1;
        
        BOOL isSantaTurn = YES;
        
        for (int i = 0; i < input.length; i++) {
            unichar direction = [input characterAtIndex:i];
            NSPoint *current = isSantaTurn ? &santa : &roboSanta;
            
            switch (direction) {
                case '^':
                    current->y++;
                    break;
                case 'v':
                    current->y--;
                    break;
                case '>':
                    current->x++;
                    break;
                case '<':
                    current->x--;
                    break;
                default:
                    break;
            }
            
            houses[[NSValue valueWithPoint:*current]] = @1;
            
            isSantaTurn = !isSantaTurn;
        }
        
        printf("Number of houses with at least one present: %lu\n", houses.count);
    }
    return 0;
}
