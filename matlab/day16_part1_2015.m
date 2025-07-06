
function main()
    mfcsam = containers.Map;
    mfcsam('children') = 3;
    mfcsam('cats') = 7;
    mfcsam('samoyeds') = 2;
    mfcsam('pomeranians') = 3;
    mfcsam('akitas') = 0;
    mfcsam('vizslas') = 0;
    mfcsam('goldfish') = 5;
    mfcsam('trees') = 3;
    mfcsam('cars') = 2;
    mfcsam('perfumes') = 1;

    fileId = fopen('input.txt', 'r');
    if fileId == -1
        error('Error opening file');
    end
    
    line = fgetl(fileId);
    while ischar(line)
        parts = strsplit(line, ' ');
        sueNumber = strrep(parts{2}, ':', '');

        matches = true;
        for i = 3:2:length(parts)
            item = strrep(parts{i}, ':', '');
            count = str2double(strrep(parts{i+1}, ',', ''));
            if ~isKey(mfcsam, item) || mfcsam(item) ~= count
                matches = false;
                break;
            end
        end

        if matches
            disp(sueNumber);
            break;
        end
        line = fgetl(fileId);
    end

    fclose(fileId);
end
