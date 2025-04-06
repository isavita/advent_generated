
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHEMICALS 100
#define MAX_NAME_LEN 10
#define MAX_INGREDIENTS 10
#define MAX_LINE_LEN 200

typedef struct {
    char name[MAX_NAME_LEN];
    long long amount;
} ChemicalAmount;

typedef struct {
    ChemicalAmount output;
    ChemicalAmount inputs[MAX_INGREDIENTS];
    int input_count;
} Reaction;

Reaction reactions[MAX_CHEMICALS];
long long surplus[MAX_CHEMICALS];
char chemical_names[MAX_CHEMICALS][MAX_NAME_LEN];
int chemical_count = 0;
int ore_id = -1;
int fuel_id = -1;

int get_chemical_id(const char* name) {
    for (int i = 0; i < chemical_count; ++i) {
        if (strcmp(chemical_names[i], name) == 0) {
            return i;
        }
    }
    if (chemical_count >= MAX_CHEMICALS) {
        fprintf(stderr, "Error: Too many chemicals\n");
        exit(1);
    }
    strcpy(chemical_names[chemical_count], name);
    if (strcmp(name, "ORE") == 0) ore_id = chemical_count;
    if (strcmp(name, "FUEL") == 0) fuel_id = chemical_count;
    return chemical_count++;
}

long long calculate_ore(int chem_id, long long amount) {
    if (chem_id == ore_id) {
        return amount;
    }

    if (surplus[chem_id] >= amount) {
        surplus[chem_id] -= amount;
        return 0;
    }

    amount -= surplus[chem_id];
    surplus[chem_id] = 0;

    Reaction* r = NULL;
    for(int i = 0; i < chemical_count; ++i) {
         // Find the reaction that produces this chemical
         // Note: Assumes chemical IDs match reaction array indices *after* parsing
         // This search is needed because reaction array isn't indexed by output chem_id directly during parsing
         if (get_chemical_id(reactions[i].output.name) == chem_id) {
              r = &reactions[i];
              break;
         }
    }
     // This should not happen with valid input if chem_id != ore_id
    if (r == NULL) {
        fprintf(stderr, "Error: Reaction not found for %s\n", chemical_names[chem_id]);
        exit(1);
    }


    long long reaction_output_amount = r->output.amount;
    long long times = (amount + reaction_output_amount - 1) / reaction_output_amount; // Ceiling division

    long long ore_needed = 0;
    for (int i = 0; i < r->input_count; ++i) {
        int input_chem_id = get_chemical_id(r->inputs[i].name);
        ore_needed += calculate_ore(input_chem_id, r->inputs[i].amount * times);
    }

    surplus[chem_id] += times * reaction_output_amount - amount;
    return ore_needed;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    int reaction_count = 0;

    // Initialize ORE
    get_chemical_id("ORE");

    while (fgets(line, sizeof(line), file)) {
        if (reaction_count >= MAX_CHEMICALS) {
             fprintf(stderr, "Error: Too many reactions\n");
             fclose(file);
             return 1;
        }

        Reaction* current_reaction = &reactions[reaction_count];
        current_reaction->input_count = 0;

        char* inputs_str = strtok(line, "=");
        char* output_str = strtok(NULL, ">");
        output_str++; // Skip leading space if present

        // Parse output
        sscanf(output_str, "%lld %s", &current_reaction->output.amount, current_reaction->output.name);
        get_chemical_id(current_reaction->output.name); // Ensure output chemical has an ID

        // Parse inputs
        char* input_token = strtok(inputs_str, ",");
        while (input_token != NULL) {
            if (current_reaction->input_count >= MAX_INGREDIENTS) {
                 fprintf(stderr, "Error: Too many ingredients for reaction\n");
                 fclose(file);
                 return 1;
            }
            ChemicalAmount* current_input = &current_reaction->inputs[current_reaction->input_count];
            sscanf(input_token, "%lld %s", &current_input->amount, current_input->name);
             get_chemical_id(current_input->name); // Ensure input chemical has an ID
            current_reaction->input_count++;
            input_token = strtok(NULL, ",");
        }
        reaction_count++; // Increment after successfully parsing the reaction
    }
    fclose(file);

    // Ensure FUEL was parsed and get its ID
    if (fuel_id == -1) {
        fprintf(stderr, "Error: FUEL reaction not found in input\n");
        return 1;
    }

    long long ore_available = 1000000000000LL;
    long long low = 0, high = ore_available; // Start high very large, refine if needed
    long long max_fuel = 0;

    // Estimate a reasonable upper bound for high
     memset(surplus, 0, sizeof(surplus));
     long long ore_for_one_fuel = calculate_ore(fuel_id, 1);
     if (ore_for_one_fuel > 0) {
         high = (ore_available / ore_for_one_fuel) * 2; // Generous starting upper bound
         if (high == 0) high = 1; // Handle case where 1 fuel costs more than available ore
     }


    while (low <= high) {
        long long mid = low + (high - low) / 2;
        if (mid == 0) { // Avoid calculating ore for 0 fuel if low becomes 0
             low = 1;
             continue;
        }

        memset(surplus, 0, sizeof(surplus)); // Reset surplus for each check
        long long ore_needed = calculate_ore(fuel_id, mid);

        if (ore_needed <= ore_available) {
            max_fuel = mid;
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }

    printf("%lld\n", max_fuel);

    return 0;
}
