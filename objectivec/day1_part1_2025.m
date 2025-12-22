
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = [[NSBundle mainBundle] pathForResource:@"input" ofType:@"txt"];
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        
        int pos = 50, cnt = 0;
        for (NSString *line in lines) {
            if (line.length < 2) continue;
            int amt = [[line substringFromIndex:1] intValue];
            pos = (pos + ([line characterAtIndex:0] == 'R' ? amt : -amt)) % 100;
            if (pos < 0) pos += 100;
            cnt += (pos == 0);
        }
        printf("%d\n", cnt);
    }
    return 0;
}
