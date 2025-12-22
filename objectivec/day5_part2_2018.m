#import <Foundation/Foundation.h>

static int react(char *s) {
    char *w = s;
    for (char *r = s; *r; ++r) {
        if (w > s && abs(*r - *(w-1)) == 32) --w;
        else *w++ = *r;
    }
    *w = 0;
    return (int)(w - s);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        if (!txt) return 1;
        NSUInteger len = txt.length;
        char *poly = malloc(len + 1);
        strcpy(poly, txt.UTF8String);

        int best = (int)len;
        for (char c = 'a'; c <= 'z'; ++c) {
            char *tmp = malloc(len + 1), *out = tmp;
            for (char *in = poly; *in; ++in)
                if (!(tolower(*in) == c || *in == c - 32)) *out++ = *in;
            *out = 0;
            int l = react(tmp);
            if (l < best) best = l;
            free(tmp);
        }
        printf("%d\n", best);
        free(poly);
    }
    return 0;
}