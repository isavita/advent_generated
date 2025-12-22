
#import <Foundation/Foundation.h>
#import <CommonCrypto/CommonCrypto.h>

static const uint32_t CACHE_SIZE = 40000;
static NSString *gCache[CACHE_SIZE];

static NSString *md5HexOfData(NSData *data) {
    unsigned char digest[CC_MD5_DIGEST_LENGTH];
    CC_MD5(data.bytes, (CC_LONG)data.length, digest);
    NSMutableString *hex = [NSMutableString stringWithCapacity:32];
    for (int i = 0; i < CC_MD5_DIGEST_LENGTH; i++)
        [hex appendFormat:@"%02x", digest[i]];
    return hex;
}

static NSString *stretchedHash(NSString *salt, NSInteger idx) {
    if (idx < 0 || idx >= CACHE_SIZE) return nil;
    if (gCache[idx]) return gCache[idx];

    NSString *msg = [NSString stringWithFormat:@"%@%ld", salt, (long)idx];
    NSString *h = md5HexOfData([msg dataUsingEncoding:NSUTF8StringEncoding]);
    for (int i = 0; i < 2016; i++)
        h = md5HexOfData([h dataUsingEncoding:NSUTF8StringEncoding]);

    gCache[idx] = [h copy];
    return gCache[idx];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *salt = [NSString stringWithContentsOfFile:path
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        salt = [salt stringByTrimmingCharactersInSet:
                [NSCharacterSet whitespaceAndNewlineCharacterSet]];

        NSInteger found = 0, index = 0;
        while (found < 64) {
            NSString *hash = stretchedHash(salt, index);
            unichar triplet = 0;
            for (int i = 0; i < 30; i++) {
                if ([hash characterAtIndex:i] == [hash characterAtIndex:i+1] &&
                    [hash characterAtIndex:i] == [hash characterAtIndex:i+2]) {
                    triplet = [hash characterAtIndex:i];
                    break;
                }
            }
            if (triplet) {
                NSString *quint = [@"" stringByPaddingToLength:5
                                                     withString:[NSString stringWithCharacters:&triplet length:1]
                                                startingAtIndex:0];
                for (int j = 1; j <= 1000; j++) {
                    NSString *nextHash = stretchedHash(salt, index + j);
                    if ([nextHash containsString:quint]) {
                        found++;
                        break;
                    }
                }
            }
            if (found == 64) {
                printf("%ld\n", (long)index);
                break;
            }
            index++;
        }

        for (NSUInteger i = 0; i < CACHE_SIZE; i++)
            if (gCache[i]) gCache[i] = nil;
    }
    return 0;
}
