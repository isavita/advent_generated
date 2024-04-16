local bots = {}
local outputs = {}
local instructions = {}

-- Function to process the bot action
local function process_bot(bot_id)
    if bots[bot_id] and #bots[bot_id] == 2 and instructions[bot_id] then
        table.sort(bots[bot_id])
        local low_value, high_value = bots[bot_id][1], bots[bot_id][2]
        local instr = instructions[bot_id]

        -- Check if the bot is the one we're looking for
        if low_value == 17 and high_value == 61 then
            print("Bot responsible for comparing 61 and 17:", bot_id)
        end

        -- Distribute low value
        if instr.low_type == "bot" then
            if not bots[instr.low_target] then bots[instr.low_target] = {} end
            table.insert(bots[instr.low_target], low_value)
            process_bot(instr.low_target)
        else
            outputs[instr.low_target] = low_value
        end

        -- Distribute high value
        if instr.high_type == "bot" then
            if not bots[instr.high_target] then bots[instr.high_target] = {} end
            table.insert(bots[instr.high_target], high_value)
            process_bot(instr.high_target)
        else
            outputs[instr.high_target] = high_value
        end

        -- Clear the bot's chips after processing
        bots[bot_id] = {}
    end
end

-- Read instructions from the file
for line in io.lines("input.txt") do
    if line:match("^value") then
        local value, bot_id = line:match("value (%d+) goes to bot (%d+)")
        value, bot_id = tonumber(value), tonumber(bot_id)
        if not bots[bot_id] then bots[bot_id] = {} end
        table.insert(bots[bot_id], value)
        if instructions[bot_id] then
            process_bot(bot_id)
        end
    else
        local bot_id, low_type, low_target, high_type, high_target =
            line:match("bot (%d+) gives low to (%a+) (%d+) and high to (%a+) (%d+)")
        bot_id, low_target, high_target = tonumber(bot_id), tonumber(low_target), tonumber(high_target)
        instructions[bot_id] = {
            low_type = low_type,
            low_target = low_target,
            high_type = high_type,
            high_target = high_target
        }
        if bots[bot_id] and #bots[bot_id] == 2 then
            process_bot(bot_id)
        end
    end
end