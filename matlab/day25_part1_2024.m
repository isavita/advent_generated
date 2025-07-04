
function main()
    fid = fopen('input.txt', 'r');
    raw = textscan(fid, '%s', 'Delimiter', '\n', 'WhiteSpace', '\r\n');
    fclose(fid);
    raw = raw{1};
    raw = raw(cellfun(@(x) ~isempty(strtrim(x)), raw));

    if mod(length(raw), 7) ~= 0
        disp(0);
        return;
    end

    locks = {};
    keys = {};

    for i = 1:7:length(raw)
        block = raw(i:min(i+6, length(raw)));
        if any(cellfun(@(x) length(x) < 5, block))
            continue;
        end
        if all(block{1} == '#')
            locks{end+1} = parse_lock(block);
        else
            keys{end+1} = parse_key(block);
        end
    end

    count = 0;
    for l = 1:length(locks)
        for k = 1:length(keys)
            if fits(locks{l}, keys{k})
                count = count + 1;
            end
        end
    end
    disp(count);
end

function h = parse_lock(b)
    h = zeros(1, 5);
    for c = 1:5
        cnt = 0;
        for r = 2:7
            if b{r}(c) == '#'
                cnt = cnt + 1;
            else
                break;
            end
        end
        h(c) = cnt;
    end
end

function h = parse_key(b)
    h = zeros(1, 5);
    for c = 1:5
        cnt = 0;
        for r = 6:-1:1
            if b{r}(c) == '#'
                cnt = cnt + 1;
            else
                break;
            end
        end
        h(c) = cnt;
    end
end

function result = fits(lock, key)
    result = all(lock + key <= 5);
end

main();
