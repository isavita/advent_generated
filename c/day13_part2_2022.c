
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

// --- Data Structure ---
typedef enum { INT_TYPE, LIST_TYPE } ItemType;

typedef struct Item {
    ItemType type;
    union {
        int int_val;
        struct {
            struct Item** elements;
            size_t size;
            size_t capacity;
        } list_val;
    } data;
} Item;

// --- Forward Declarations ---
Item* parse_packet(const char** s);
void free_item(Item* item);
int compare_items(const Item* a, const Item* b);

// --- Memory Management ---
void* Malloc(size_t size) {
    void* ptr = malloc(size);
    if (!ptr && size > 0) {
        perror("malloc failed");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

void* Realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (!new_ptr && size > 0) {
        perror("realloc failed");
        exit(EXIT_FAILURE);
    }
    return new_ptr;
}

// --- Parser ---
Item* create_int_item(int value) {
    Item* item = (Item*)Malloc(sizeof(Item));
    item->type = INT_TYPE;
    item->data.int_val = value;
    return item;
}

Item* create_list_item(size_t capacity) {
    Item* item = (Item*)Malloc(sizeof(Item));
    item->type = LIST_TYPE;
    item->data.list_val.size = 0;
    item->data.list_val.capacity = capacity > 0 ? capacity : 4; // Initial capacity
    item->data.list_val.elements = (Item**)Malloc(item->data.list_val.capacity * sizeof(Item*));
    return item;
}

void add_list_element(Item* list_item, Item* element) {
    if (list_item->type != LIST_TYPE) return;
    if (list_item->data.list_val.size >= list_item->data.list_val.capacity) {
        list_item->data.list_val.capacity *= 2;
        list_item->data.list_val.elements = (Item**)Realloc(list_item->data.list_val.elements, list_item->data.list_val.capacity * sizeof(Item*));
    }
    list_item->data.list_val.elements[list_item->data.list_val.size++] = element;
}

Item* parse_packet(const char** s) {
    while (isspace(**s)) (*s)++;

    if (isdigit(**s)) {
        char* endptr;
        long val = strtol(*s, &endptr, 10);
        *s = endptr;
        return create_int_item((int)val);
    } else if (**s == '[') {
        (*s)++; // Skip '['
        Item* list_item = create_list_item(4);
        while (isspace(**s)) (*s)++;
        if (**s == ']') {
            (*s)++; // Skip ']' for empty list
            return list_item;
        }
        while (1) {
            Item* element = parse_packet(s);
            add_list_element(list_item, element);
            while (isspace(**s)) (*s)++;
            if (**s == ']') {
                (*s)++; // Skip ']'
                break;
            } else if (**s == ',') {
                (*s)++; // Skip ','
                while (isspace(**s)) (*s)++;
            } else {
                // Error: unexpected character
                fprintf(stderr, "Parse error: unexpected char %c\n", **s);
                 free_item(list_item); // Clean up partially built list
                 exit(EXIT_FAILURE);
            }
        }
        return list_item;
    } else {
         fprintf(stderr, "Parse error: unexpected char %c at start\n", **s);
         exit(EXIT_FAILURE); // Should not happen with valid input
    }
}

// --- Comparison ---
int compare_items(const Item* a, const Item* b) {
    if (a->type == INT_TYPE && b->type == INT_TYPE) {
        return (a->data.int_val < b->data.int_val) ? -1 : (a->data.int_val > b->data.int_val);
    }

    if (a->type == LIST_TYPE && b->type == LIST_TYPE) {
        size_t min_len = a->data.list_val.size < b->data.list_val.size ? a->data.list_val.size : b->data.list_val.size;
        for (size_t i = 0; i < min_len; ++i) {
            int cmp = compare_items(a->data.list_val.elements[i], b->data.list_val.elements[i]);
            if (cmp != 0) return cmp;
        }
        return (a->data.list_val.size < b->data.list_val.size) ? -1 : (a->data.list_val.size > b->data.list_val.size);
    }

    if (a->type == INT_TYPE && b->type == LIST_TYPE) {
        // Temporarily treat 'a' as a list [a]
        Item* temp_a_list = create_list_item(1);
        Item* temp_a_int = create_int_item(a->data.int_val); // Create a copy for the list
        add_list_element(temp_a_list, temp_a_int);
        int result = compare_items(temp_a_list, b);
        // No need to free temp_a_int as free_item(temp_a_list) will handle it
        free_item(temp_a_list);
        return result;
    }

    if (a->type == LIST_TYPE && b->type == INT_TYPE) {
        // Temporarily treat 'b' as a list [b]
        Item* temp_b_list = create_list_item(1);
        Item* temp_b_int = create_int_item(b->data.int_val); // Create a copy for the list
        add_list_element(temp_b_list, temp_b_int);
        int result = compare_items(a, temp_b_list);
         // No need to free temp_b_int as free_item(temp_b_list) will handle it
        free_item(temp_b_list);
        return result;
    }

    return 0; // Should not happen
}


// --- qsort Wrapper ---
int compare_items_qsort(const void* p1, const void* p2) {
    const Item* item1 = *(const Item**)p1;
    const Item* item2 = *(const Item**)p2;
    return compare_items(item1, item2);
}

// --- Cleanup ---
void free_item(Item* item) {
    if (!item) return;
    if (item->type == LIST_TYPE) {
        for (size_t i = 0; i < item->data.list_val.size; ++i) {
            free_item(item->data.list_val.elements[i]);
        }
        free(item->data.list_val.elements);
    }
    free(item);
}

// --- Main ---
int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    Item** packets = NULL;
    size_t packets_count = 0;
    size_t packets_capacity = 0;
    char line[1024]; // Max line length assumed

    while (fgets(line, sizeof(line), f)) {
        size_t len = strlen(line);
        // Remove trailing newline if present
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }

        // Skip empty lines (between pairs)
        if (len == 0) continue;

        // Resize packet array if needed
        if (packets_count >= packets_capacity) {
            packets_capacity = packets_capacity == 0 ? 16 : packets_capacity * 2;
            packets = (Item**)Realloc(packets, packets_capacity * sizeof(Item*));
        }

        const char* ptr = line;
        packets[packets_count++] = parse_packet(&ptr);
    }
    fclose(f);

    // Add divider packets
    const char* div1_str = "[[2]]";
    const char* div2_str = "[[6]]";
    Item* divider1 = parse_packet(&div1_str);
    Item* divider2 = parse_packet(&div2_str);

    if (packets_count + 2 > packets_capacity) {
        packets_capacity = packets_count + 2;
        packets = (Item**)Realloc(packets, packets_capacity * sizeof(Item*));
    }
    packets[packets_count++] = divider1;
    packets[packets_count++] = divider2;

    // Sort packets
    qsort(packets, packets_count, sizeof(Item*), compare_items_qsort);

    // Find divider positions (1-based index)
    int divider1_pos = -1;
    int divider2_pos = -1;
    for (size_t i = 0; i < packets_count; ++i) {
        // Compare content, not pointers, as temporary items were created during comparison
        if (compare_items(packets[i], divider1) == 0) {
            divider1_pos = i + 1;
        }
        if (compare_items(packets[i], divider2) == 0) {
            divider2_pos = i + 1;
        }
        if (divider1_pos != -1 && divider2_pos != -1) {
             // Optimization: Break early if both found.
             // Note: This assumes dividers are unique in the final list,
             // which is true given the problem context. If they could be
             // identical to other packets, we'd need to ensure we find
             // the specific divider instances added earlier, perhaps by
             // comparing pointers instead of content IF we didn't create
             // temporary items in the compare function. Since we do create
             // temporary items, content comparison is necessary here.
             break;
        }
    }


    // Calculate and print result
    if (divider1_pos != -1 && divider2_pos != -1) {
         printf("%d\n", divider1_pos * divider2_pos);
    } else {
        fprintf(stderr, "Error: Could not find divider packets after sorting.\n");
    }

    // Cleanup
    for (size_t i = 0; i < packets_count; ++i) {
        // Note: divider1 and divider2 are inside the packets array now,
        // they will be freed by this loop.
        free_item(packets[i]);
    }
    free(packets);

    return EXIT_SUCCESS;
}
