
function day09
% DAY09  Disk Fragmenter – Part 1
% Reads input.txt, compacts the disk, and prints the checksum.

    % 1. Read the disk map --------------------------------------------------
    str = fileread('input.txt');
    str = strtrim(str);               % remove trailing newline
    digits = str - '0';             % convert to numeric vector

    % 2. Build the initial block list -------------------------------------
    blocks = [];
    fileID = 0;
    for k = 1:numel(digits)
        len = digits(k);
        if mod(k,2) == 1            % file block
            blocks(end+1:end+len) = fileID;
            fileID = fileID + 1;
        else                        % free space
            blocks(end+1:end+len) = -1;
        end
    end

    % 3. Compact the disk ---------------------------------------------------
    left = 1;
    right = numel(blocks);
    while left < right
        % Find left-most free space
        while left <= right && blocks(left) ~= -1
            left = left + 1;
        end
        % Find right-most file block
        while left <= right && blocks(right) == -1
            right = right - 1;
        end
        if left < right
            blocks(left) = blocks(right);
            blocks(right) = -1;
            left  = left  + 1;
            right = right - 1;
        end
    end

    % 4. Compute checksum ---------------------------------------------------
    checksum = 0;
    for pos = 1:numel(blocks)
        if blocks(pos) ~= -1
            checksum = checksum + (pos-1) * blocks(pos);
        end
    end

    % 5. Output -------------------------------------------------------------
    fprintf('%d\n', checksum);
end
