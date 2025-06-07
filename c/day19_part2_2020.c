
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_RULES 150
#define MAX_OPTIONS_PER_RULE 2
#define MAX_SUBRULES_PER_OPTION 3
#define MAX_MESSAGES 500

typedef struct {
    int sub_rules[MAX_SUBRULES_PER_OPTION];
    int sub_rule_count;
} Option;

typedef struct {
    char** resolved_strings;
    int resolved_count;
    int resolved_capacity;
    Option options[MAX_OPTIONS_PER_RULE];
    int option_count;
} Rule;

Rule g_rules[MAX_RULES] = {0};
char* g_messages[MAX_MESSAGES] = {0};
int g_message_count = 0;

void add_resolved_string(Rule* rule, const char* str) {
    if (rule->resolved_count >= rule->resolved_capacity) {
        rule->resolved_capacity = (rule->resolved_capacity == 0) ? 8 : rule->resolved_capacity * 2;
        rule->resolved_strings = realloc(rule->resolved_strings, rule->resolved_capacity * sizeof(char*));
    }
    rule->resolved_strings[rule->resolved_count++] = strdup(str);
}

void parse_input(char* input) {
    char* separator = strstr(input, "\n\n");
    if (!separator) return;
    *separator = '\0';
    char* rule_part = input;
    char* message_part = separator + 2;

    char* line = strtok(rule_part, "\n");
    while (line) {
        char* p = line;
        int rule_num = strtol(p, &p, 10);
        p += 2;

        if (*p == '"') {
            char terminal_str[2] = {p[1], '\0'};
            add_resolved_string(&g_rules[rule_num], terminal_str);
        } else {
            while (*p) {
                Option* opt = &g_rules[rule_num].options[g_rules[rule_num].option_count++];
                opt->sub_rule_count = 0;
                while (*p && *p != '|') {
                    if (isdigit(*p)) {
                        opt->sub_rules[opt->sub_rule_count++] = strtol(p, &p, 10);
                    } else {
                        p++;
                    }
                }
                if (*p == '|') p++;
            }
        }
        line = strtok(NULL, "\n");
    }

    line = strtok(message_part, "\n");
    while (line) {
        g_messages[g_message_count++] = strdup(line);
        line = strtok(NULL, "\n");
    }
}

void fill_in_graph(int rule_num) {
    Rule* rule = &g_rules[rule_num];
    if (rule->resolved_count > 0) return;

    for (int i = 0; i < rule->option_count; ++i) {
        Option* opt = &rule->options[i];
        
        char** current_resolved = malloc(sizeof(char*));
        current_resolved[0] = strdup("");
        int current_count = 1;

        for (int j = 0; j < opt->sub_rule_count; ++j) {
            int sub_rule_num = opt->sub_rules[j];
            fill_in_graph(sub_rule_num);
            Rule* sub_rule = &g_rules[sub_rule_num];

            char** next_resolved = malloc(current_count * sub_rule->resolved_count * sizeof(char*));
            int next_count = 0;

            for (int k = 0; k < current_count; ++k) {
                for (int l = 0; l < sub_rule->resolved_count; ++l) {
                    size_t len = strlen(current_resolved[k]) + strlen(sub_rule->resolved_strings[l]) + 1;
                    next_resolved[next_count] = malloc(len);
                    snprintf(next_resolved[next_count], len, "%s%s", current_resolved[k], sub_rule->resolved_strings[l]);
                    next_count++;
                }
                free(current_resolved[k]);
            }
            free(current_resolved);
            current_resolved = next_resolved;
            current_count = next_count;
        }

        for (int j = 0; j < current_count; ++j) {
            add_resolved_string(rule, current_resolved[j]);
            free(current_resolved[j]);
        }
        free(current_resolved);
    }
}

int compare_str_ptr(const void* a, const void* b) {
    return strcmp(*(const char**)a, *(const char**)b);
}

int solve() {
    fill_in_graph(42);
    fill_in_graph(31);

    qsort(g_rules[42].resolved_strings, g_rules[42].resolved_count, sizeof(char*), compare_str_ptr);
    qsort(g_rules[31].resolved_strings, g_rules[31].resolved_count, sizeof(char*), compare_str_ptr);

    size_t chunk_len = strlen(g_rules[42].resolved_strings[0]);
    int match_count = 0;

    for (int i = 0; i < g_message_count; ++i) {
        char* msg = g_messages[i];
        size_t msg_len = strlen(msg);
        if (msg_len % chunk_len != 0) continue;

        int num_chunks = msg_len / chunk_len;
        if (num_chunks < 3) continue;

        int count42 = 0;
        int count31 = 0;
        
        char chunk[chunk_len + 1];
        chunk[chunk_len] = '\0';
        char* p_chunk = chunk;
        int k = 0;

        for (; k < num_chunks; ++k) {
            strncpy(chunk, msg + k * chunk_len, chunk_len);
            if (!bsearch(&p_chunk, g_rules[42].resolved_strings, g_rules[42].resolved_count, sizeof(char*), compare_str_ptr)) break;
            count42++;
        }
        
        for (; k < num_chunks; ++k) {
            strncpy(chunk, msg + k * chunk_len, chunk_len);
            if (!bsearch(&p_chunk, g_rules[31].resolved_strings, g_rules[31].resolved_count, sizeof(char*), compare_str_ptr)) break;
            count31++;
        }

        if (count42 + count31 == num_chunks && count31 >= 1 && count42 > count31) {
            match_count++;
        }
    }
    return match_count;
}

void free_resources() {
    for (int i = 0; i < MAX_RULES; ++i) {
        if (g_rules[i].resolved_strings) {
            for (int j = 0; j < g_rules[i].resolved_count; ++j) {
                free(g_rules[i].resolved_strings[j]);
            }
            free(g_rules[i].resolved_strings);
        }
    }
    for (int i = 0; i < g_message_count; ++i) {
        free(g_messages[i]);
    }
}

int main(void) {
    FILE* file = fopen("input.txt", "r");
    if (!file) return 1;

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* input = malloc(size + 2);
    fread(input, 1, size, file);
    input[size] = '\n';
    input[size + 1] = '\0';
    fclose(file);

    parse_input(input);
    int result = solve();
    printf("%d\n", result);

    free(input);
    free_resources();

    return 0;
}
