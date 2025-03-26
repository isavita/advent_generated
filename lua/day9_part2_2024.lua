
local function solve()
    local f = io.open("input.txt", "r")
    if not f then
        error("Could not open input.txt")
        return
    end
    local line = f:read("*l")
    f:close()
    line = line:match("^%s*(.-)%s*$") -- Equivalent to strip()

    local disk = {}
    local file_id_counter = 0
    local is_file = true

    for i = 1, #line do
        local length = tonumber(line:sub(i, i))
        local value
        if is_file then
            value = tostring(file_id_counter)
            file_id_counter = file_id_counter + 1
        else
            value = '.'
        end
        for _ = 1, length do
            table.insert(disk, value)
        end
        is_file = not is_file
    end

    local files = {}
    local current_file_id_str = nil
    local segment_start = 0

    for i, val in ipairs(disk) do
        if val == '.' then
            current_file_id_str = nil
        else
            if val ~= current_file_id_str then
                current_file_id_str = val
                segment_start = i
            end

            if i == #disk or disk[i + 1] ~= val then
                 table.insert(files, {
                    id = tonumber(val),
                    start = segment_start, -- 1-based index
                    end_ = i           -- 1-based index (use end_ as 'end' is keyword)
                })
            end
        end
    end

    for file_idx = #files, 1, -1 do
        local file = files[file_idx]
        local file_len = file.end_ - file.start + 1
        local best_span_start = -1
        local current_span_len = 0
        local current_span_start = -1

        -- Find leftmost suitable free space before current file start
        for i = 1, file.start - 1 do
            if disk[i] == '.' then
                if current_span_len == 0 then
                    current_span_start = i
                end
                current_span_len = current_span_len + 1
                if current_span_len == file_len then
                    best_span_start = current_span_start
                    break -- Found the leftmost exact match
                end
            else
                current_span_len = 0
                current_span_start = -1
            end
        end

        if best_span_start ~= -1 then
            -- Clear old location
            for i = file.start, file.end_ do
                disk[i] = '.'
            end
            -- Fill new location
            local file_id_str = tostring(file.id)
            for i = 0, file_len - 1 do
                disk[best_span_start + i] = file_id_str
            end
        end
    end

    local checksum = 0
    for i, val in ipairs(disk) do
        if val ~= '.' then
            -- Use (i-1) for 0-based index in calculation like Python
            checksum = checksum + (i - 1) * tonumber(val)
        end
    end
    print(checksum)
end

solve()
