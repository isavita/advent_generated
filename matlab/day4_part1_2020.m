
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    passportData = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', ' \t\n');
    fclose(fid);

    passportData = passportData{1};
    passports = {};
    currentPassport = '';

    for i = 1:length(passportData)
        line = passportData{i};
        if isempty(line)
            if ~isempty(currentPassport)
                passports{end+1} = strtrim(currentPassport);
                currentPassport = '';
            end
        else
            currentPassport = [currentPassport, ' ', line];
        end
    end

    if ~isempty(currentPassport)
        passports{end+1} = strtrim(currentPassport);
    end

    validPassports = 0;
    requiredFields = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'};

    for i = 1:length(passports)
        passport = passports{i};
        isValid = true;
        for j = 1:length(requiredFields)
            if isempty(strfind(passport, requiredFields{j}))
                isValid = false;
                break;
            end
        end
        if isValid
            validPassports = validPassports + 1;
        end
    end

    fprintf('%d\n', validPassports);
end

main();
