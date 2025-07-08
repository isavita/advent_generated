
function main()
    MOD = 2^24;
    NUM_STEPS = 2000;
    
    fid = fopen('input.txt', 'r');
    initials = fscanf(fid, '%d');
    fclose(fid);
    
    buyers = cell(length(initials), 1);
    for i = 1:length(initials)
        s = initials(i);
        prices = zeros(NUM_STEPS + 1, 1);
        for j = 0:NUM_STEPS
            prices(j+1) = mod(s, 10);
            if j < NUM_STEPS
                s = nextSecret(s, MOD);
            end
        end
        
        changes = diff(prices);
        buyers{i} = struct('prices', prices, 'changes', changes);
    end
    
    PATTERN_COUNT = 19^4;
    globalSum = zeros(PATTERN_COUNT, 1);
    
    for i = 1:length(buyers)
        b = buyers{i};
        localPrice = -ones(PATTERN_COUNT, 1);
        
        for j = 1:(NUM_STEPS - 3)
            c1 = b.changes(j);
            c2 = b.changes(j+1);
            c3 = b.changes(j+2);
            c4 = b.changes(j+3);
            
            if c1 >= -9 && c1 <= 9 && c2 >= -9 && c2 <= 9 && c3 >= -9 && c3 <= 9 && c4 >= -9 && c4 <= 9
                idx = encodeChange4(c1, c2, c3, c4);
                if localPrice(idx+1) < 0
                    localPrice(idx+1) = b.prices(j+4);
                end
            end
        end
        
        validIndices = localPrice >= 0;
        globalSum(validIndices) = globalSum(validIndices) + localPrice(validIndices);
    end
    
    ans = max(globalSum);
    fprintf('%d\n', ans);
end

function s = nextSecret(s, MOD)
    x = s * 64;
    s = bitxor(s, x);
    s = bitand(s, MOD - 1);
    x = floor(s / 32);
    s = bitxor(s, x);
    s = bitand(s, MOD - 1);
    x = s * 2048;
    s = bitxor(s, x);
    s = bitand(s, MOD - 1);
end

function idx = encodeChange4(c1, c2, c3, c4)
    idx = (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
end

main();
