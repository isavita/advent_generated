#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCrypto.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            return 1;
        }
        
        NSString *secretKey = [data stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSInteger number = 0;
        
        while (1) {
            NSString *input = [NSString stringWithFormat:@"%@%ld", secretKey, (long)number];
            unsigned char digest[CC_MD5_DIGEST_LENGTH];
            CC_MD5(input.UTF8String, (CC_LONG)input.length, digest);
            
            NSMutableString *hashString = [NSMutableString stringWithCapacity:CC_MD5_DIGEST_LENGTH * 2];
            for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++) {
                [hashString appendFormat:@"%02x", digest[i]];
            }
            
            if ([hashString hasPrefix:@"00000"]) {
                NSLog(@"%ld", (long)number);
                break;
            }
            number++;
        }
    }
    return 0;
}