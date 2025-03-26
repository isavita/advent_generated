
local function main()
    local workflows = {}
    local variable_indices = { x = 1, m = 2, a = 3, s = 4 }
    local memo = {}

    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening input.txt")
        return
    end

    local parsing_workflows = true
    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$") -- trim whitespace

        if line == "" then
            parsing_workflows = false
        elseif parsing_workflows then
            local name, rules_str = line:match("^(%w+){(.-)}$")
            if name then
                local rules = {}
                for rule_str in rules_str:gmatch("([^,]+)") do
                    local condition, destination = rule_str:match("^(.-):(.+)$")
                    if condition then
                        table.insert(rules, { condition = condition, destination = destination })
                    else
                        table.insert(rules, { condition = nil, destination = rule_str })
                    end
                end
                workflows[name] = rules
            end
        -- else: Ignore parts section for this problem
        end
    end
    file:close()

    local function count_combinations(constraints)
        local total = 1
        for i = 1, 4 do
            local count = constraints[i][2] - constraints[i][1] + 1
            if count <= 0 then return 0 end
            total = total * count
        end
        return total
    end

    local function copy_constraints(constraints)
        local new_constraints = {}
        for i = 1, 4 do
            new_constraints[i] = { constraints[i][1], constraints[i][2] }
        end
        return new_constraints
    end

    local process -- Forward declaration for recursion

    process = function(workflow_name, constraints)
        -- Check for invalid ranges early
         for i = 1, 4 do
             if constraints[i][1] > constraints[i][2] then return 0 end
         end

        -- Create a unique key for memoization
        local key_parts = { workflow_name }
         for i = 1, 4 do
             table.insert(key_parts, constraints[i][1])
             table.insert(key_parts, constraints[i][2])
         end
        local key = table.concat(key_parts, ",")

        if memo[key] then
            return memo[key]
        end

        local rules = workflows[workflow_name]
        local current_result = 0
        local current_constraints = copy_constraints(constraints) -- Use a copy we can modify

        for _, rule in ipairs(rules) do
            if rule.condition then
                local var, op, val_str = rule.condition:match("([xmas])([<>])(%d+)")
                local value = tonumber(val_str)
                local var_idx = variable_indices[var]
                local var_range = current_constraints[var_idx]

                local true_range, false_range
                if op == '>' then
                    true_range = { math.max(var_range[1], value + 1), var_range[2] }
                    false_range = { var_range[1], math.min(var_range[2], value) }
                elseif op == '<' then
                    true_range = { var_range[1], math.min(var_range[2], value - 1) }
                    false_range = { math.max(var_range[1], value), var_range[2] }
                end

                -- Process true branch
                if true_range[1] <= true_range[2] then
                    local next_constraints = copy_constraints(current_constraints)
                    next_constraints[var_idx] = true_range
                    if rule.destination == 'A' then
                        current_result = current_result + count_combinations(next_constraints)
                    elseif rule.destination ~= 'R' then
                        current_result = current_result + process(rule.destination, next_constraints)
                    end
                end

                -- Update current constraints for the false branch (next rule)
                if false_range[1] <= false_range[2] then
                     current_constraints[var_idx] = false_range
                else
                    -- No possibility for this or subsequent rules, exit loop
                    goto continue_workflow -- Using goto to break inner logic and continue outer loop processing
                end

            else -- Default rule (no condition)
                if rule.destination == 'A' then
                    current_result = current_result + count_combinations(current_constraints)
                elseif rule.destination ~= 'R' then
                    current_result = current_result + process(rule.destination, current_constraints)
                end
                -- This default rule consumes all remaining possibilities
                 goto continue_workflow -- Using goto to break inner logic and continue outer loop processing
            end
        end
        ::continue_workflow:: -- Label for goto

        memo[key] = current_result
        return current_result
    end

    local initial_constraints = { { 1, 4000 }, { 1, 4000 }, { 1, 4000 }, { 1, 4000 } }
    local total = process('in', initial_constraints)
    print(total)
end

main()
