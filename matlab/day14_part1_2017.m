
function main()
    fid = fopen('input.txt', 'r');
    keyString = fgetl(fid);
    fclose(fid);

    totalUsed = 0;
    for i = 0:127
        rowKey = sprintf('%s-%d', keyString, i);
        hash = knotHash(rowKey);

        for j = 1:length(hash)
            val = hex2dec(hash(j));
            binStr = dec2bin(val, 4);
            totalUsed = totalUsed + sum(binStr == '1');
        end
    end

    fprintf('%d\n', totalUsed);
end

function hexHash = knotHash(inputStr)
    lengths = double(inputStr);
    additional = [17, 31, 73, 47, 23];
    lengths = [lengths, additional];

    list = 0:255;
    n = 256;

    position = 0;
    skip = 0;

    for round = 1:64
        for k = 1:length(lengths)
            len = lengths(k);
            
            start_0based = mod(position, n);

            for m = 0:floor(len / 2) - 1
                idx1_0based = mod(start_0based + m, n);
                idx2_0based = mod(start_0based + len - 1 - m, n);

                idx1_1based = idx1_0based + 1;
                idx2_1based = idx2_0based + 1;

                temp = list(idx1_1based);
                list(idx1_1based) = list(idx2_1based);
                list(idx2_1based) = temp;
            end
            
            position = position + len + skip;
            skip = skip + 1;
        end
    end

    denseHash = zeros(1, 16);
    for i = 0:15
        xor_val = 0;
        for j = 0:15
            xor_val = bitxor(xor_val, list(i * 16 + j + 1));
        end
        denseHash(i + 1) = xor_val;
    end

    hexHash = sprintf('%02x', denseHash);
end
