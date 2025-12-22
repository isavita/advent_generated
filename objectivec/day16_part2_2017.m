#import <Foundation/Foundation.h>

#define PROGRAMS_LEN 16

static void spin(char *p, int x) {
    char t[PROGRAMS_LEN];
    memcpy(t, p, PROGRAMS_LEN);
    for (int i = 0; i < PROGRAMS_LEN; ++i)
        p[(i + x) % PROGRAMS_LEN] = t[i];
}

static void exchange(char *p, int a, int b) {
    char t = p[a]; p[a] = p[b]; p[b] = t;
}

static void partner(char *p, char a, char b) {
    int ia = -1, ib = -1;
    for (int i = 0; i < PROGRAMS_LEN; ++i) {
        if (p[i] == a) ia = i;
        if (p[i] == b) ib = i;
    }
    if (ia >= 0 && ib >= 0) exchange(p, ia, ib);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *err = nil;
        NSString *whole = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:&err];
        if (!whole) { NSLog(@"%@", err); return 1; }
        NSString *trim = [whole stringByTrimmingCharactersInSet:
                          [NSCharacterSet whitespaceAndNewlineCharacterSet]];
        NSArray *moves = [trim componentsSeparatedByString:@","];

        char p[] = "abcdefghijklmnop";
        char initial[PROGRAMS_LEN+1];
        strcpy(initial, p);
        int cycle = 0;

        for (int i = 0; i < 1000000000; ++i) {
            for (NSString *m in moves) {
                unichar op = [m characterAtIndex:0];
                if (op == 's') {
                    spin(p, [[m substringFromIndex:1] intValue]);
                } else if (op == 'x') {
                    NSArray *ab = [[m substringFromIndex:1] componentsSeparatedByString:@"/"];
                    exchange(p, [ab[0] intValue], [ab[1] intValue]);
                } else if (op == 'p') {
                    partner(p, [m characterAtIndex:1], [m characterAtIndex:3]);
                }
            }
            if (cycle == 0 && strcmp(p, initial) == 0) {
                cycle = i + 1;
                break;
            }
        }

        if (cycle) {
            int rem = 1000000000 % cycle;
            strcpy(p, initial);
            for (int i = 0; i < rem; ++i) {
                for (NSString *m in moves) {
                    unichar op = [m characterAtIndex:0];
                    if (op == 's') {
                        spin(p, [[m substringFromIndex:1] intValue]);
                    } else if (op == 'x') {
                        NSArray *ab = [[m substringFromIndex:1] componentsSeparatedByString:@"/"];
                        exchange(p, [ab[0] intValue], [ab[1] intValue]);
                    } else if (op == 'p') {
                        partner(p, [m characterAtIndex:1], [m characterAtIndex:3]);
                    }
                }
            }
        }

        printf("%s\n", p);
    }
    return 0;
}