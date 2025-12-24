#import <Foundation/Foundation.h>

#define MAX_RULES 200
#define MAX_SUBRULES 3
#define MAX_MESSAGES 500
#define MAX_MSG_LEN 150
#define MAX_LINE_LEN 200

typedef struct {
    int ids[MAX_SUBRULES];
    int count;
} SubRuleSeq;

typedef enum { RULE_INVALID, RULE_CHAR, RULE_SEQ, RULE_ALT } RuleType;

typedef struct {
    RuleType type;
    char character;
    SubRuleSeq seq1;
    SubRuleSeq seq2;
    BOOL defined;
} Rule;

static Rule rules[MAX_RULES];
static char messages[MAX_MESSAGES][MAX_MSG_LEN];
static int message_count = 0;
static int max_rule_num = -1;

static int match_rule(int rule_id, const char *message, int pos, int msg_len) {
    if (pos < 0 || pos > msg_len) return -1;
    if (rule_id < 0 || rule_id > max_rule_num || !rules[rule_id].defined) return -1;

    Rule *rule = &rules[rule_id];
    switch (rule->type) {
        case RULE_CHAR:
            return (pos < msg_len && message[pos] == rule->character) ? pos + 1 : -1;

        case RULE_SEQ: {
            int cur = pos;
            for (int i = 0; i < rule->seq1.count; ++i) {
                cur = match_rule(rule->seq1.ids[i], message, cur, msg_len);
                if (cur == -1) return -1;
            }
            return cur;
        }

        case RULE_ALT: {
            int cur = pos;
            for (int i = 0; i < rule->seq1.count; ++i) {
                cur = match_rule(rule->seq1.ids[i], message, cur, msg_len);
                if (cur == -1) break;
            }
            if (cur != -1) return cur;

            cur = pos;
            for (int i = 0; i < rule->seq2.count; ++i) {
                cur = match_rule(rule->seq2.ids[i], message, cur, msg_len);
                if (cur == -1) return -1;
            }
            return cur;
        }

        default:
            return -1;
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        for (int i = 0; i < MAX_RULES; ++i) {
            rules[i].type = RULE_INVALID;
            rules[i].defined = NO;
            rules[i].seq1.count = 0;
            rules[i].seq2.count = 0;
        }

        NSString *path = [[NSFileManager defaultManager] currentDirectoryPath];
        NSString *file = [path stringByAppendingPathComponent:@"input.txt"];
        NSString *content = [NSString stringWithContentsOfFile:file
                                                     encoding:NSUTF8StringEncoding
                                                        error:nil];
        if (!content) return 1;

        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        NSUInteger idx = 0;
        NSString *line;

        // Rules
        while (idx < lines.count && (line = [lines[idx] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet]).length) {
            NSArray *parts = [line componentsSeparatedByString:@":"];
            if (parts.count < 2) { idx++; continue; }
            int rule_id = [parts[0] intValue];
            if (rule_id < 0 || rule_id >= MAX_RULES) { idx++; continue; }
            if (rule_id > max_rule_num) max_rule_num = rule_id;
            Rule *rule = &rules[rule_id];
            rule->defined = YES;

            NSString *def = [[parts[1] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
            if ([def rangeOfString:@"|"].location != NSNotFound) {
                rule->type = RULE_ALT;
                NSArray *alts = [def componentsSeparatedByString:@"|"];
                NSString *s1 = [alts[0] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
                NSString *s2 = [alts[1] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
                NSArray *ids1 = [s1 componentsSeparatedByString:@" "];
                NSArray *ids2 = [s2 componentsSeparatedByString:@" "];
                for (NSString *id in ids1) if (rule->seq1.count < MAX_SUBRULES) rule->seq1.ids[rule->seq1.count++] = [id intValue];
                for (NSString *id in ids2) if (rule->seq2.count < MAX_SUBRULES) rule->seq2.ids[rule->seq2.count++] = [id intValue];
            } else if (def.length == 1 && [[NSCharacterSet letterCharacterSet] characterIsMember:[def characterAtIndex:0]]) {
                rule->type = RULE_CHAR;
                rule->character = [def UTF8String][0];
            } else {
                rule->type = RULE_SEQ;
                NSArray *ids = [def componentsSeparatedByString:@" "];
                for (NSString *id in ids) if (rule->seq1.count < MAX_SUBRULES) rule->seq1.ids[rule->seq1.count++] = [id intValue];
            }
            idx++;
        }

        idx++; // skip empty line

        // Messages
        while (idx < lines.count && message_count < MAX_MESSAGES) {
            line = [lines[idx] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
            if (line.length && line.length < MAX_MSG_LEN) {
                strcpy(messages[message_count++], [line UTF8String]);
            }
            idx++;
        }

        int valid = 0;
        for (int i = 0; i < message_count; ++i) {
            int len = (int)strlen(messages[i]);
            if (match_rule(0, messages[i], 0, len) == len) valid++;
        }

        printf("%d\n", valid);
    }
    return 0;
}