
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Cannot open input.txt');
    end
    
    buyers = [];
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if ~isempty(line)
            buyers = [buyers, str2double(line)];
        end
    end
    fclose(fid);
    
    total = 0;
    for b = buyers
        s = uint64(b);
        for i = 1:2000
            s = nextSecret(s);
        end
        total = total + s;
    end
    
    disp(total);
end

function s = nextSecret(s)
    x = bitshift(s, 6);
    s = bitxor(s, x);
    s = bitand(s, uint64(16777215));
    x = bitshift(s, -5);
    s = bitxor(s, x);
    s = bitand(s, uint64(16777215));
    x = bitshift(s, 11);
    s = bitxor(s, x);
    s = bitand(s, uint64(16777215));
end

main();
