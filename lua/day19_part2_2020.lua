
--[[
--- Day 19: Monster Messages ---

Solution for Advent of Code 2020 Day 19 parts 1 and 2.
Reads rules and messages from input.txt, validates messages against rule 0,
and prints the count of valid messages for both parts.
--]]

local rules = {}
local messages = {}

-- Parses a rule string and adds it to the global 'rules' table.
-- Handles both character rules (e.g., "a") and sub-rule sequences/alternatives.
local function parse_rule(line)
    local id_str, definition = line:match("^(%d+): (.*)$")
    if not id_str then return end -- Skip potentially empty lines or invalid formats

    local id = tonumber(id_str)
    local char_match = definition:match('^"(%a)"$')

    if char_match then
        -- Store the single character for literal rules
        rules[id] = char_match
    else
        -- Store alternatives as a list of sequences (which are lists of rule IDs)
        local alternatives = {}
        for alt_part in definition:gmatch("[^|]+") do
            local sequence = {}
            for num_str in alt_part:gmatch("%d+") do
                table.insert(sequence, tonumber(num_str))
            end
            table.insert(alternatives, sequence)
        end
        rules[id] = alternatives
    end
end

-- Reads the input file, parsing rules and messages.
local function read_input(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Error: Cannot open input file '" .. filename .. "'")
    end

    local reading_rules = true
    for line in file:lines() do
        if line == "" then
            reading_rules = false -- Switch to reading messages after blank line
        elseif reading_rules then
            parse_rule(line)
        else
            table.insert(messages, line)
        end
    end
    file:close()
end

-- Memoization table for the match function.
-- Key format: "rule_id:index"
-- Value: Table of possible end indices (1-based).
local memo = {}

-- Recursive function to check if a message substring matches a given rule.
-- Uses memoization to avoid redundant calculations.
--
-- Args:
--   message (string): The message string being validated.
--   index (number): The starting index (1-based) in the message to match from.
--   rule_id (number): The ID of the rule to match.
--
-- Returns:
--   table: A list of possible end indices (1-based) in the message after a
--          successful match. An empty table indicates no match.
local function match(message, index, rule_id)
    -- Check memoization cache first
    local memo_key = rule_id .. ":" .. index
    if memo[memo_key] then
        return memo[memo_key]
    end

    -- Prevent matching beyond the message length for character rules
    if index > #message then
        memo[memo_key] = {}
        return {}
    end

    local rule = rules[rule_id]
    local possible_ends = {}
    local unique_ends = {} -- Use as a set to store unique end indices

    if type(rule) == "string" then
        -- Base case: Match a single character
        if message:sub(index, index) == rule then
             -- Only one possible end: the next index
             if not unique_ends[index + 1] then
                 table.insert(possible_ends, index + 1)
                 unique_ends[index + 1] = true
             end
        end
    else
        -- Recursive case: Match alternatives (|) of sequences
        for _, sequence in ipairs(rule) do
            -- Start matching the sequence from the current index
            local current_indices = {index}

            for _, sub_rule_id in ipairs(sequence) do
                local next_indices = {}
                local unique_next = {} -- Set for next indices within this sequence step

                -- Try matching the sub-rule from all possible current positions
                for _, current_idx in ipairs(current_indices) do
                    local results = match(message, current_idx, sub_rule_id)
                    for _, end_idx in ipairs(results) do
                        -- Add unique resulting indices to the list for the next sub-rule
                        if not unique_next[end_idx] then
                             table.insert(next_indices, end_idx)
                             unique_next[end_idx] = true
                        end
                    end
                end

                current_indices = next_indices -- Pass results to the next sub-rule
                -- If any sub-rule in the sequence fails to match, this sequence fails
                if #current_indices == 0 then break end
            end

            -- Add the final indices reached by this successful alternative sequence
            for _, end_idx in ipairs(current_indices) do
                if not unique_ends[end_idx] then
                     table.insert(possible_ends, end_idx)
                     unique_ends[end_idx] = true
                end
            end
        end
    end

    -- Store result in memoization cache and return
    memo[memo_key] = possible_ends
    return possible_ends
end

-- Checks if a message completely matches the specified start rule.
local function check_message(message, start_rule_id)
    memo = {} -- IMPORTANT: Reset memoization cache for each new message
    local end_indices = match(message, 1, start_rule_id)

    -- Check if any match consumed the entire message
    for _, end_idx in ipairs(end_indices) do
        if end_idx == #message + 1 then
            return true
        end
    end
    return false
end

-- Solves Part 1: Count messages matching rule 0 with original rules.
local function solve_part1()
    local count = 0
    for _, msg in ipairs(messages) do
        if check_message(msg, 0) then
            count = count + 1
        end
    end
    return count
end

-- Solves Part 2: Modify rules 8 and 11, then count matching messages.
-- This part relies on the specific structure of rules 0, 8, 11, 42, and 31
-- after the modification:
-- 0: 8 11
-- 8: 42 | 42 8   => One or more 42s (42+)
-- 11: 42 31 | 42 11 31 => N 42s followed by N 31s (42{N} 31{N}, N >= 1)
-- Combined: Rule 0 matches 42{M} 31{N} where M > N >= 1.
-- We assume rules 42 and 31 match fixed-length, non-overlapping chunks.
local function solve_part2()
    -- Apply rule changes for Part 2
    -- Note: The 'rules' table is modified globally here.
    rules[8] = {{42}, {42, 8}}
    rules[11] = {{42, 31}, {42, 11, 31}}

    -- Determine the fixed length of strings matched by rules 42 and 31.
    -- We find one example match and assume all matches have the same length.
    local chunk_len = -1
    for _, msg in ipairs(messages) do
         memo = {} -- Reset memo for length finding
         local ends42 = match(msg, 1, 42)
         if #ends42 > 0 then
              -- Check if all matches have the same length
              local len = ends42[1] - 1
              local consistent = true
              for i = 2, #ends42 do
                  if ends42[i] - 1 ~= len then consistent = false; break end
              end
              if consistent and len > 0 then
                  chunk_len = len
                  break -- Found the length
              end
         end
    end

    if chunk_len <= 0 then
        error("Error: Could not determine a consistent, positive match length for rule 42.")
        -- Alternative: Try rule 31 or require manual length input if needed.
    end

    -- Helper to check if a chunk fully matches a given rule ID
    local function check_chunk(chunk, rule_id)
         memo = {} -- Reset memo for each chunk check
         local ends = match(chunk, 1, rule_id)
         for _, e in ipairs(ends) do
             if e == #chunk + 1 then return true end
         end
         return false
    end


    local count = 0
    for _, msg in ipairs(messages) do
        -- Message length must be a multiple of chunk_len and contain at least 3 chunks (m > n >= 1 implies m+n >= 3)
        if #msg > 0 and #msg % chunk_len == 0 and #msg >= 3 * chunk_len then
            local num_chunks = #msg / chunk_len
            local matches42 = 0
            local matches31 = 0
            local state = "matching42" -- State machine: matching42 -> matching31
            local possible_match = true

            for i = 1, num_chunks do
                local chunk = msg:sub((i - 1) * chunk_len + 1, i * chunk_len)

                if state == "matching42" then
                    if check_chunk(chunk, 42) then
                        matches42 = matches42 + 1
                    elseif check_chunk(chunk, 31) then
                        -- Transition to matching 31s
                        state = "matching31"
                        matches31 = matches31 + 1
                    else
                        -- Invalid chunk
                        possible_match = false
                        break
                    end
                elseif state == "matching31" then
                    if check_chunk(chunk, 31) then
                        matches31 = matches31 + 1
                    else
                        -- Invalid chunk (must be 31 here)
                        possible_match = false
                        break
                    end
                end
            end

            -- Final validation: M > N and N >= 1, and we ended in state matching31
            if possible_match and state == "matching31" and matches31 >= 1 and matches42 > matches31 then
                count = count + 1
            end
        end
    end

    return count
end


-- Main execution block
local function main()
    read_input("input.txt")

    -- Make a deep copy of rules if needed between parts, though Part 2 modifies globally
    -- local original_rules = {} -- Example deep copy (adjust if rules contain tables)
    -- for k, v in pairs(rules) do original_rules[k] = v end

    print("--- Day 19: Monster Messages ---")

    local part1_result = solve_part1()
    print("Part 1: Number of messages matching rule 0 =", part1_result)

    -- Rules are modified globally by solve_part2()
    local part2_result = solve_part2()
    print("Part 2: Number of messages matching rule 0 (with updated rules) =", part2_result)
end

-- Run the program
main()
