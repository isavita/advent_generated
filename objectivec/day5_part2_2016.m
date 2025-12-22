#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCrypto.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *doorID = [[NSString stringWithContentsOfFile:@"input.txt"
                                                     encoding:NSUTF8StringEncoding
                                                        error:nil]
                           stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
        char password[9] = {0};
        bool found[8] = {false};
        int filled = 0;
        unsigned long long idx = 0;
        const char *hex = "0123456789abcdef";
        while (filled < 8) {
            NSString *candidate = [doorID stringByAppendingFormat:@"%llu", idx];
            const char *cstr = [candidate UTF8String];
            unsigned char digest[CC_MD5_DIGEST_LENGTH];
            CC_MD5(cstr, (CC_LONG)strlen(cstr), digest);
            if (digest[0] == 0 && digest[1] == 0 && (digest[2] & 0xF0) == 0) {
                int pos = digest[2] & 0x0F;
                if (pos < 8 && !found[pos]) {
                    found[pos] = true;
                    password[pos] = hex[(digest[3] & 0xF0) >> 4];
                    filled++;
                }
            }
            idx++;
        }
        printf("%s\n", password);
    }
    return 0;
}