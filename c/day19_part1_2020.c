
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

// --- Constants ---
#define MAX_RULES 200       // Max number of rules expected
#define MAX_SUBRULES 3      // Max sub-rules per sequence part (e.g., "1 2 3")
#define MAX_MESSAGES 500    // Max number of messages expected
#define MAX_MSG_LEN 150     // Max length of a message
#define MAX_LINE_LEN 200    // Max length of a line in the input file

// --- Data Structures ---

// Represents a sequence of sub-rule IDs (e.g., "1 2")
typedef struct {
    int ids[MAX_SUBRULES];
    int count;
} SubRuleSeq;

// Represents a single rule
typedef struct {
    enum { RULE_INVALID, RULE_CHAR, RULE_SEQ, RULE_ALT } type;
    char character;     // For RULE_CHAR
    SubRuleSeq seq1;    // For RULE_SEQ or first part of RULE_ALT
    SubRuleSeq seq2;    // For second part of RULE_ALT (optional)
    bool defined;       // Flag to check if the rule was defined in the input
} Rule;

// Global storage for rules and messages
Rule rules[MAX_RULES];
char messages[MAX_MESSAGES][MAX_MSG_LEN];
int message_count = 0;
int max_rule_num = -1;

// --- Function Prototypes ---
bool parse_rules(FILE *fp);
void parse_line(char *line);
int match_rule(int rule_id, const char *message, int pos, int msg_len);

// --- Parsing Functions ---

// Reads the rule section from the file pointer
bool parse_rules(FILE *fp) {
    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), fp)) {
        // Remove trailing newline
        line[strcspn(line, "\r\n")] = 0;

        // Empty line signifies end of rules section
        if (strlen(line) == 0) {
            return true; // Successfully finished reading rules
        }
        parse_line(line);
    }
    return false; // Reached EOF before finding empty line (or error)
}

// Parses a single rule line
void parse_line(char *line) {
    char *colon = strchr(line, ':');
    if (!colon) return; // Invalid format

    *colon = '\0'; // Split line at colon
    int rule_id = atoi(line);
    if (rule_id < 0 || rule_id >= MAX_RULES) {
        fprintf(stderr, "Error: Rule ID %d out of bounds (0-%d)\n", rule_id, MAX_RULES - 1);
        return;
    }
    if (rule_id > max_rule_num) {
        max_rule_num = rule_id;
    }

    Rule *rule = &rules[rule_id];
    rule->defined = true;
    char *definition = colon + 1;

    // Trim leading space
    while (isspace((unsigned char)*definition)) definition++;

    // Check for character rule (e.g., "a")
    if (*definition == '"') {
        rule->type = RULE_CHAR;
        rule->character = definition[1]; // Character is after the quote
        return;
    }

    // Check for alternative rule (contains '|')
    char *pipe = strchr(definition, '|');
    if (pipe) {
        rule->type = RULE_ALT;
        *pipe = '\0'; // Split definition at pipe
        char *part1 = definition;
        char *part2 = pipe + 1;

        // Parse first sequence
        char *token = strtok(part1, " ");
        while (token) {
            if (rule->seq1.count < MAX_SUBRULES) {
                rule->seq1.ids[rule->seq1.count++] = atoi(token);
            } // Else: Too many subrules, ignore extra
            token = strtok(NULL, " ");
        }

        // Parse second sequence
        token = strtok(part2, " ");
        while (token) {
             if (rule->seq2.count < MAX_SUBRULES) {
                rule->seq2.ids[rule->seq2.count++] = atoi(token);
            } // Else: Too many subrules, ignore extra
           token = strtok(NULL, " ");
        }
    } else {
        // Must be a sequence rule
        rule->type = RULE_SEQ;
        char *token = strtok(definition, " ");
        while (token) {
             if (rule->seq1.count < MAX_SUBRULES) {
                rule->seq1.ids[rule->seq1.count++] = atoi(token);
            } // Else: Too many subrules, ignore extra
           token = strtok(NULL, " ");
        }
    }
}

// --- Matching Function ---

// Tries to match rule `rule_id` against `message` starting at `pos`.
// Returns the position *after* a successful match, or -1 if no match.
// IMPORTANT: This implementation works for Part 1 (no loops). It returns
// only the *first* successful match found for alternatives.
int match_rule(int rule_id, const char *message, int pos, int msg_len) {
    // Boundary checks
    if (pos < 0 || pos > msg_len) { // Allow pos == msg_len for empty matches (not applicable here)
        return -1;
    }
     if (rule_id < 0 || rule_id > max_rule_num || !rules[rule_id].defined) {
        fprintf(stderr, "Error: Accessing undefined or invalid rule %d\n", rule_id);
        return -1; // Undefined rule
    }

    Rule *rule = &rules[rule_id];

    switch (rule->type) {
        case RULE_CHAR:
            if (pos < msg_len && message[pos] == rule->character) {
                return pos + 1; // Match, consume one char
            } else {
                return -1; // No match
            }

        case RULE_SEQ: {
            int current_pos = pos;
            for (int i = 0; i < rule->seq1.count; ++i) {
                current_pos = match_rule(rule->seq1.ids[i], message, current_pos, msg_len);
                if (current_pos == -1) {
                    return -1; // Sub-rule failed, sequence fails
                }
            }
            return current_pos; // Sequence matched successfully
        }

        case RULE_ALT: {
            // Try first alternative (seq1)
            int current_pos1 = pos;
            bool seq1_ok = true;
            for (int i = 0; i < rule->seq1.count; ++i) {
                current_pos1 = match_rule(rule->seq1.ids[i], message, current_pos1, msg_len);
                if (current_pos1 == -1) {
                    seq1_ok = false;
                    break;
                }
            }
            if (seq1_ok) {
                return current_pos1; // First alternative matched
            }

            // Try second alternative (seq2)
            int current_pos2 = pos; // Reset position
            bool seq2_ok = true;
            for (int i = 0; i < rule->seq2.count; ++i) {
                current_pos2 = match_rule(rule->seq2.ids[i], message, current_pos2, msg_len);
                if (current_pos2 == -1) {
                    seq2_ok = false;
                    break;
                }
            }
             if (seq2_ok) {
                return current_pos2; // Second alternative matched
            }

            return -1; // Neither alternative matched
        }

        case RULE_INVALID:
        default:
             fprintf(stderr, "Error: Encountered invalid rule type for rule %d\n", rule_id);
             return -1;
    }
}


// --- Main Function ---
int main() {
    // Initialize rules array
    for (int i = 0; i < MAX_RULES; ++i) {
        rules[i].type = RULE_INVALID;
        rules[i].defined = false;
        rules[i].seq1.count = 0;
        rules[i].seq2.count = 0;
    }

    // Open input file
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    // Parse rules section
    if (!parse_rules(fp)) {
         fprintf(stderr, "Error parsing rules section or missing separator.\n");
         fclose(fp);
         return 1;
    }

    // Read messages section
    char line[MAX_LINE_LEN];
    while (message_count < MAX_MESSAGES && fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\r\n")] = 0; // Remove trailing newline
        if (strlen(line) > 0 && strlen(line) < MAX_MSG_LEN) {
            strcpy(messages[message_count++], line);
        } else if (strlen(line) >= MAX_MSG_LEN) {
             fprintf(stderr, "Warning: Message too long, skipping: %s...\n", line);
        }
    }

    fclose(fp);

    // Validate messages against rule 0
    int valid_count = 0;
    if (!rules[0].defined) {
        fprintf(stderr, "Error: Rule 0 is not defined in the input.\n");
        return 1;
    }

    for (int i = 0; i < message_count; ++i) {
        int msg_len = strlen(messages[i]);
        int end_pos = match_rule(0, messages[i], 0, msg_len);

        // A message is valid if rule 0 matches and consumes the *entire* message
        if (end_pos == msg_len) {
            valid_count++;
        }
    }

    // Print the result
    printf("%d\n", valid_count);

    return 0;
}
