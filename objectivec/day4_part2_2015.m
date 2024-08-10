#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonDigest.h>

NSString *md5Hash(NSString *input) {
    const char *cStr = [input UTF8String];
    unsigned char digest[CC_MD5_DIGEST_LENGTH];
    CC_MD5(cStr, (CC_LONG)strlen(cStr), digest);

    NSMutableString *output = [NSMutableString stringWithCapacity:CC_MD5_DIGEST_LENGTH * 2];
    for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++) {
        [output appendFormat:@"%02x", digest[i]];
    }
    return output;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read the secret key from input.txt
        NSString *filePath = @"input.txt";
        NSString *secretKey = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];

        if (!secretKey) {
            NSLog(@"Error reading input file.");
            return 1;
        }

        // Part One: Find the lowest number that produces a hash starting with five zeroes
        int number = 0;
        while (1) {
            NSString *input = [NSString stringWithFormat:@"%@%d", secretKey, number];
            NSString *hash = md5Hash(input);
            if ([hash hasPrefix:@"00000"]) {
                NSLog(@"Part One: %d", number);
                break;
            }
            number++;
        }

        // Part Two: Find the lowest number that produces a hash starting with six zeroes
        number = 0;
        while (1) {
            NSString *input = [NSString stringWithFormat:@"%@%d", secretKey, number];
            NSString *hash = md5Hash(input);
            if ([hash hasPrefix:@"000000"]) {
                NSLog(@"Part Two: %d", number);
                break;
            }
            number++;
        }
    }
    return 0;
}