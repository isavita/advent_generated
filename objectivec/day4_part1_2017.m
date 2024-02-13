#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *passphrases = [input componentsSeparatedByString:@"\n"];
        
        int validPassphrases = 0;
        
        for (NSString *passphrase in passphrases) {
            NSArray *words = [passphrase componentsSeparatedByString:@" "];
            NSCountedSet *wordSet = [NSCountedSet setWithArray:words];
            
            if (wordSet.count == words.count) {
                validPassphrases++;
            }
        }
        
        printf("%d\n", validPassphrases);
    }
    return 0;
}