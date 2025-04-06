
#!/bin/bash

# Define the main function
main() {
    # Use awk for processing the file and calculations
    awk '
    # Helper functions for min/max
    function max(a, b) { return (a > b) ? a : b }
    function min(a, b) { return (a < b) ? a : b }

    # Function to calculate combinations in a set of ranges
    function count_combinations(ranges,    x1, x2, m1, m2, a1, a2, s1, s2, count) {
        x1 = ranges[0,0]; x2 = ranges[0,1]
        m1 = ranges[1,0]; m2 = ranges[1,1]
        a1 = ranges[2,0]; a2 = ranges[2,1]
        s1 = ranges[3,0]; s2 = ranges[3,1]

        if (x1 > x2 || m1 > m2 || a1 > a2 || s1 > s2) return 0

        count = (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)
        # awk numbers can be large, direct multiplication is fine
        return count
    }

    # Recursive processing function with memoization
    function process(wf_name, ranges,   key, i, rule, condition, dest, var_idx, op, val,
                                        current_val_range_min, current_val_range_max,
                                        true_range_min, true_range_max, false_range_min, false_range_max,
                                        new_ranges_true, new_ranges_false, result, sub_result, n,
                                        x1, x2, m1, m2, a1, a2, s1, s2) {

        x1 = ranges[0,0]; x2 = ranges[0,1]
        m1 = ranges[1,0]; m2 = ranges[1,1]
        a1 = ranges[2,0]; a2 = ranges[2,1]
        s1 = ranges[3,0]; s2 = ranges[3,1]

        # Base case: invalid range
        if (x1 > x2 || m1 > m2 || a1 > a2 || s1 > s2) return 0

        # Memoization key
        key = wf_name SUBSEP x1 SUBSEP x2 SUBSEP m1 SUBSEP m2 SUBSEP a1 SUBSEP a2 SUBSEP s1 SUBSEP s2
        if (key in memo) {
            return memo[key]
        }

        result = 0
        n = num_rules[wf_name]

        # Copy ranges to modify locally for the false branch propagation
        for(i=0; i<4; i++) {
             new_ranges_false[i,0] = ranges[i,0]
             new_ranges_false[i,1] = ranges[i,1]
        }

        for (i = 1; i <= n; i++) {
            condition = rule_cond[wf_name, i]
            dest = rule_dest[wf_name, i]

            if (condition == "") {
                # Unconditional rule (last rule)
                if (dest == "A") {
                    result += count_combinations(new_ranges_false)
                } else if (dest == "R") {
                    # Add nothing
                } else {
                    result += process(dest, new_ranges_false)
                }
                break # This rule consumes the rest, stop processing
            } else {
                 # Conditional rule
                var_idx = rule_var_idx[wf_name, i]
                op = rule_op[wf_name, i]
                val = rule_val[wf_name, i]

                current_val_range_min = new_ranges_false[var_idx, 0]
                current_val_range_max = new_ranges_false[var_idx, 1]

                # Determine true/false ranges based on operator
                if (op == ">") {
                    true_range_min = max(current_val_range_min, val + 1)
                    true_range_max = current_val_range_max
                    false_range_min = current_val_range_min
                    false_range_max = min(current_val_range_max, val)
                } else { # op == "<"
                    true_range_min = current_val_range_min
                    true_range_max = min(current_val_range_max, val - 1)
                    false_range_min = max(current_val_range_min, val)
                    false_range_max = current_val_range_max
                }

                # Process the "true" branch if its range is valid
                if (true_range_min <= true_range_max) {
                     # Create new ranges for the true branch
                     for(vr=0; vr<4; vr++) {
                         new_ranges_true[vr,0] = new_ranges_false[vr,0]
                         new_ranges_true[vr,1] = new_ranges_false[vr,1]
                     }
                     new_ranges_true[var_idx, 0] = true_range_min
                     new_ranges_true[var_idx, 1] = true_range_max

                     if (dest == "A") {
                         result += count_combinations(new_ranges_true)
                     } else if (dest == "R") {
                         # Add nothing
                     } else {
                         result += process(dest, new_ranges_true)
                     }
                }

                # Update the current ranges for the next rule (false branch)
                if (false_range_min <= false_range_max) {
                    new_ranges_false[var_idx, 0] = false_range_min
                    new_ranges_false[var_idx, 1] = false_range_max
                    # Continue to the next rule with the modified range
                } else {
                    # False branch range is empty, no need to check further rules
                    break
                }
            }
        }

        memo[key] = result
        return result
    }

    # Parse workflows block
    /^[^\{]/ && NF {
        gsub(/[{}]/, " ") # Remove braces
        wf_name = $1
        rules_str = $2
        n = split(rules_str, rules_arr, ",")
        num_rules[wf_name] = n
        for (i = 1; i <= n; i++) {
            rule = rules_arr[i]
            if (index(rule, ":")) {
                split(rule, parts, ":")
                condition = parts[1]
                dest = parts[2]
                var = substr(condition, 1, 1)
                op = substr(condition, 2, 1)
                val = substr(condition, 3) + 0 # Ensure numeric conversion
                rule_cond[wf_name, i] = condition
                rule_dest[wf_name, i] = dest
                rule_var_idx[wf_name, i] = index("xmas", var) - 1 # 0=x, 1=m, 2=a, 3=s
                rule_op[wf_name, i] = op
                rule_val[wf_name, i] = val
            } else {
                # Unconditional rule
                rule_cond[wf_name, i] = ""
                rule_dest[wf_name, i] = rule
            }
        }
        in_workflows = 1
    }

    # Stop parsing after the empty line separating workflows and parts
    /^$/ {
        if (in_workflows) exit # Exit awk script blocks processing, go to END
    }

    # END block executes after processing the input (or exiting early)
    END {
        # Initial ranges [1, 4000] for x, m, a, s
        initial_ranges[0,0] = 1; initial_ranges[0,1] = 4000 # x
        initial_ranges[1,0] = 1; initial_ranges[1,1] = 4000 # m
        initial_ranges[2,0] = 1; initial_ranges[2,1] = 4000 # a
        initial_ranges[3,0] = 1; initial_ranges[3,1] = 4000 # s

        # Start processing from the "in" workflow
        total_combinations = process("in", initial_ranges)

        # Print the final result with integer format to avoid scientific notation
        printf "%.0f\n", total_combinations
    }
    ' input.txt # Feed input.txt to awk
}

# Execute the main function
main
