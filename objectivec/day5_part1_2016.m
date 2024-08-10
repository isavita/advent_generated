#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCrypto.h>

NSString *md5Hash(NSString *input) {
    const char *ptr = [input UTF8String];
    unsigned char hash[CC_MD5_DIGEST_LENGTH];
    CC_MD5(ptr, (CC_LONG)strlen(ptr), hash);
    NSMutableString *output = [NSMutableString stringWithCapacity:CC_MD5_DIGEST_LENGTH * 2];
    for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++) {
        [output appendFormat:@"%02x", hash[i]];
    }
    return output;
}

NSString *findPassword(NSString *doorID) {
    NSMutableString *password = [NSMutableString string];
    for (int i = 0; password.length < 8; i++) {
        NSString *hash = md5Hash([NSString stringWithFormat:@"%@%d", doorID, i]);
        if ([hash hasPrefix:@"00000"]) {
            [password appendFormat:@"%c", [hash characterAtIndex:5]];
        }
    }
    return password;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *doorID = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            return 1;
        }
        doorID = [doorID stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSString *password = findPassword(doorID);
        NSLog(@"%@", password);
    }
    return 0;
}