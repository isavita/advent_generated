
function main()
    fid = fopen('input.txt', 'r');
    passphrases = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    passphrases = passphrases{1};

    valid_count = 0;
    for i = 1:length(passphrases)
        words = strsplit(passphrases{i});
        if length(words) == length(unique(words))
            valid_count = valid_count + 1;
        end
    end
    disp(valid_count);
end

main();
