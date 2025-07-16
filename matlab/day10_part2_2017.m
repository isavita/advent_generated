
function main()
    fileID = fopen('input.txt', 'r');
    input_str = fgetl(fileID);
    fclose(fileID);

    lengths = double(input_str);
    suffix = [17, 31, 73, 47, 23];
    lengths = [lengths, suffix];

    list_size = 256;
    list = uint8(0:list_size-1);
    current_position = 0;
    skip_size = 0;

    for round = 1:64
        for i = 1:numel(lengths)
            len = lengths(i);
            
            indices = mod(current_position + (0:len-1), list_size);
            
            % MATLAB uses 1-based indexing
            list(indices + 1) = flip(list(indices + 1));
            
            current_position = mod(current_position + len + skip_size, list_size);
            skip_size = skip_size + 1;
        end
    end

    dense_hash = zeros(1, 16, 'uint8');
    for i = 1:16
        block_start = (i-1) * 16 + 1;
        block_end = i * 16;
        block = list(block_start:block_end);
        
        xor_val = uint8(0);
        for k = 1:16
            xor_val = bitxor(xor_val, block(k));
        end
        dense_hash(i) = xor_val;
    end

    hex_hash = sprintf('%02x', dense_hash);
    fprintf('%s\n', hex_hash);
end
