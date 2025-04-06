
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 256
#define INITIAL_CAPACITY 16
#define VERTEX_NAME_LEN 8 // Max length for vertex names like "abc" + null terminator

// --- Hash Map for Vertex Names (Simple Linear Probing) ---
typedef struct {
    char name[VERTEX_NAME_LEN];
    int id;
    bool occupied;
} VertexEntry;

VertexEntry* vertex_map = NULL;
int vertex_map_size = 0;
int vertex_map_capacity = 0;
int vertex_count = 0;

unsigned int hash_function(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; // hash * 33 + c
    return hash;
}

void resize_vertex_map() {
    int old_capacity = vertex_map_capacity;
    VertexEntry* old_map = vertex_map;

    vertex_map_capacity = (vertex_map_capacity == 0) ? INITIAL_CAPACITY : vertex_map_capacity * 2;
    vertex_map = (VertexEntry*)calloc(vertex_map_capacity, sizeof(VertexEntry));
    if (!vertex_map) {
        perror("Failed to allocate vertex map");
        exit(EXIT_FAILURE);
    }
    vertex_map_size = 0; // Size reset, entries will be re-added

    for (int i = 0; i < old_capacity; ++i) {
        if (old_map[i].occupied) {
            unsigned int h = hash_function(old_map[i].name) % vertex_map_capacity;
            while (vertex_map[h].occupied) {
                h = (h + 1) % vertex_map_capacity;
            }
            vertex_map[h] = old_map[i]; // Copy entry
            vertex_map_size++;
        }
    }
    free(old_map);
}

int get_vertex_id(const char *name) {
    if (vertex_map_size * 2 >= vertex_map_capacity) {
        resize_vertex_map();
    }

    unsigned int h = hash_function(name) % vertex_map_capacity;
    while (vertex_map[h].occupied) {
        if (strcmp(vertex_map[h].name, name) == 0) {
            return vertex_map[h].id;
        }
        h = (h + 1) % vertex_map_capacity;
    }

    // Not found, add new vertex
    strncpy(vertex_map[h].name, name, VERTEX_NAME_LEN - 1);
    vertex_map[h].name[VERTEX_NAME_LEN - 1] = '\0';
    vertex_map[h].id = vertex_count++;
    vertex_map[h].occupied = true;
    vertex_map_size++;
    return vertex_map[h].id;
}

// --- Graph Representation ---
typedef struct {
    int u;
    int v;
    bool active; // Used to simulate edge removal
    int id;      // Unique edge ID
} Edge;

typedef struct {
    int neighbor_id;
    int edge_id;     // Index into the global edges array
} AdjNode;

typedef struct {
    AdjNode *nodes;
    int count;
    int capacity;
} AdjList;

Edge *edges = NULL;
int edge_count = 0;
int edge_capacity = 0;

AdjList *adj = NULL;
int adj_capacity = 0; // Corresponds to vertex_count

void ensure_adj_capacity(int min_capacity) {
    if (min_capacity > adj_capacity) {
        int new_capacity = (adj_capacity == 0) ? INITIAL_CAPACITY : adj_capacity;
        while (new_capacity < min_capacity) {
            new_capacity *= 2;
        }
        adj = (AdjList*)realloc(adj, new_capacity * sizeof(AdjList));
        if (!adj) {
             perror("Failed to realloc adjacency list");
             exit(EXIT_FAILURE);
        }
        // Initialize new lists
        for (int i = adj_capacity; i < new_capacity; ++i) {
            adj[i].nodes = NULL;
            adj[i].count = 0;
            adj[i].capacity = 0;
        }
        adj_capacity = new_capacity;
    }
}

void add_adj_node(int u, int neighbor_id, int edge_id) {
    ensure_adj_capacity(u + 1);
    if (adj[u].count >= adj[u].capacity) {
        adj[u].capacity = (adj[u].capacity == 0) ? 4 : adj[u].capacity * 2;
        adj[u].nodes = (AdjNode*)realloc(adj[u].nodes, adj[u].capacity * sizeof(AdjNode));
         if (!adj[u].nodes) {
             perror("Failed to realloc adjacency list nodes");
             exit(EXIT_FAILURE);
        }
    }
    adj[u].nodes[adj[u].count].neighbor_id = neighbor_id;
    adj[u].nodes[adj[u].count].edge_id = edge_id;
    adj[u].count++;
}

void add_edge(int u, int v) {
    if (edge_count >= edge_capacity) {
        edge_capacity = (edge_capacity == 0) ? INITIAL_CAPACITY : edge_capacity * 2;
        edges = (Edge*)realloc(edges, edge_capacity * sizeof(Edge));
         if (!edges) {
             perror("Failed to realloc edges");
             exit(EXIT_FAILURE);
        }
    }
    int current_edge_id = edge_count;
    edges[current_edge_id].u = u;
    edges[current_edge_id].v = v;
    edges[current_edge_id].active = true;
    edges[current_edge_id].id = current_edge_id;
    edge_count++;

    add_adj_node(u, v, current_edge_id);
    add_adj_node(v, u, current_edge_id);
}


// --- BFS ---
typedef struct {
    bool found;
    int* parent_edge; // Stores edge_id used to reach node
} BfsResult;

BfsResult breadth_first_search(int start_node, int end_node) {
    BfsResult result = {false, NULL};
    if (start_node >= vertex_count || (end_node != -1 && end_node >= vertex_count)) {
        return result; // Invalid nodes
    }

    result.parent_edge = (int*)malloc(vertex_count * sizeof(int));
    bool* visited = (bool*)calloc(vertex_count, sizeof(bool));
    int* queue = (int*)malloc(vertex_count * sizeof(int));
     if (!result.parent_edge || !visited || !queue) {
        perror("Failed to allocate BFS structures");
        free(result.parent_edge); free(visited); free(queue);
        exit(EXIT_FAILURE);
    }

    for(int i = 0; i < vertex_count; ++i) result.parent_edge[i] = -1;

    int head = 0, tail = 0;
    queue[tail++] = start_node;
    visited[start_node] = true;

    while (head < tail) {
        int current = queue[head++];

        if (end_node != -1 && current == end_node) {
            result.found = true;
            break; // Found the target
        }

        ensure_adj_capacity(current + 1); // Ensure adj list exists
        for (int i = 0; i < adj[current].count; ++i) {
            AdjNode neighbor_info = adj[current].nodes[i];
            int edge_id = neighbor_info.edge_id;
            int neighbor = neighbor_info.neighbor_id;

            if (edges[edge_id].active && !visited[neighbor]) {
                visited[neighbor] = true;
                result.parent_edge[neighbor] = edge_id;
                queue[tail++] = neighbor;
            }
        }
    }

    // If end_node is -1, we just want the reachability info (parent_edge array)
    if (end_node == -1) {
       result.found = true; // Indicate BFS completed
    }

    free(visited);
    free(queue);

    // If end_node was specified but not found, free parent_edge array
    if (end_node != -1 && !result.found) {
         free(result.parent_edge);
         result.parent_edge = NULL;
    }

    return result;
}


// --- Path Reconstruction ---
// Returns number of edges in path, stores edge ids in path_edges array
int reconstruct_path(int start_node, int end_node, int* parent_edge, int* path_edges, int max_path_len) {
    int count = 0;
    int current = end_node;
    while (current != start_node && parent_edge[current] != -1 && count < max_path_len) {
        int edge_id = parent_edge[current];
        path_edges[count++] = edge_id;
        int u = edges[edge_id].u;
        int v = edges[edge_id].v;
        current = (current == u) ? v : u; // Move to the parent node
    }
    // Reverse the path_edges array
    for (int i = 0; i < count / 2; ++i) {
        int temp = path_edges[i];
        path_edges[i] = path_edges[count - 1 - i];
        path_edges[count - 1 - i] = temp;
    }
    return count;
}


// --- Solve ---
long long solve() {
    int min_cut = 3;
    int source_node = 0; // Arbitrarily pick the first node
    int cut_component_size = -1;

    int* path_buffer = (int*)malloc(vertex_count * sizeof(int)); // Buffer for path edges
     if (!path_buffer) {
        perror("Failed to allocate path buffer");
        exit(EXIT_FAILURE);
    }

    for (int end_node = 1; end_node < vertex_count; ++end_node) {
         // Reset edge states for this pair (simulates copy_graph)
        for (int i = 0; i < edge_count; ++i) {
            edges[i].active = true;
        }

        int paths_found = 0;
        for (int k = 0; k < min_cut; ++k) {
            BfsResult bfs_res = breadth_first_search(source_node, end_node);
            if (!bfs_res.found) {
                free(bfs_res.parent_edge); // Clean up if path not found
                break; // Cannot find required number of paths
            }

            int path_len = reconstruct_path(source_node, end_node, bfs_res.parent_edge, path_buffer, vertex_count);
            for(int i = 0; i < path_len; ++i) {
                edges[path_buffer[i]].active = false; // Disable edge
            }
            free(bfs_res.parent_edge); // Free after use
            paths_found++;
        }


        if (paths_found == min_cut) {
            // Check if source and end are now disconnected
            BfsResult connectivity_check = breadth_first_search(source_node, end_node);
             bool disconnected = !connectivity_check.found;
             free(connectivity_check.parent_edge); // parent_edge not needed here, free it

            if (disconnected) {
                // Found the cut! Now find the size of the source component.
                BfsResult component_bfs = breadth_first_search(source_node, -1); // -1 means find all reachable
                int count = 0;
                for (int i = 0; i < vertex_count; ++i) {
                    // Reachable if it's the source or has a parent edge
                    if (i == source_node || component_bfs.parent_edge[i] != -1) {
                        count++;
                    }
                }
                cut_component_size = count;
                free(component_bfs.parent_edge);
                break; // Exit the outer loop
            }
        }
    }

    free(path_buffer);

    if (cut_component_size != -1) {
        long long size1 = cut_component_size;
        long long size2 = vertex_count - size1;
        return size1 * size2;
    }

    return -1; // Should not happen based on problem description
}

// --- Main ---
int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        char *vertex_str = strtok(line, ":");
        if (!vertex_str) continue;

        char *neighbors_str = strtok(NULL, ""); // Get the rest of the line
        if (!neighbors_str) continue;

        // Trim leading space if present
        while (*neighbors_str == ' ') neighbors_str++;

        int u = get_vertex_id(vertex_str);

        char *neighbor_tok = strtok(neighbors_str, " ");
        while (neighbor_tok) {
            int v = get_vertex_id(neighbor_tok);
            // Add edge only if u < v to avoid duplicates, adjacency list handles both directions
            // Correction: Need to add all edges as defined, graph building handles it.
             add_edge(u, v);
            neighbor_tok = strtok(NULL, " ");
        }
    }
    fclose(file);

    // --- Deduplicate Edges ---
    // The parse logic might add edges twice if listed symmetrically.
    // Let's filter duplicates based on (u, v) pairs regardless of order.
    // A simpler way is to just let the add_edge logic handle it,
    // the BFS will naturally traverse correctly via adjacency lists.
    // The current add_edge adds one edge struct and adds to both u and v's adj lists.

    long long result = solve();
    printf("%lld\n", result);

    // --- Cleanup ---
    free(edges);
    if (adj) {
        for (int i = 0; i < adj_capacity; ++i) {
             free(adj[i].nodes);
        }
        free(adj);
    }
    // Free vertex map (simple linear probing cleanup)
    free(vertex_map);


    return 0;
}

