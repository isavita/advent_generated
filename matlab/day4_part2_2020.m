
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    passport = '';
    validPassports = 0;

    while ~feof(fid)
        line = strtrim(fgets(fid));
        if isempty(line)
            if isValidPassport(passport)
                validPassports = validPassports + 1;
            end
            passport = '';
        else
            if ~isempty(passport)
                passport = [passport ' '];
            end
            passport = [passport line];
        end
    end

    if ~isempty(passport) && isValidPassport(passport)
        validPassports = validPassports + 1;
    end

    fprintf('%d\n', validPassports);
    fclose(fid);
end

function valid = isValidPassport(passport)
    fields = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'};
    requiredFields = length(fields);
    foundFields = 0;

    for i = 1:requiredFields
        field = fields{i};
        pattern = [field ':(?<value>\S+)'];
        match = regexp(passport, pattern, 'names');

        if ~isempty(match)
            value = match.value;
            validField = false;
            switch field
                case 'byr'
                    validField = validateYear(value, 1920, 2002);
                case 'iyr'
                    validField = validateYear(value, 2010, 2020);
                case 'eyr'
                    validField = validateYear(value, 2020, 2030);
                case 'hgt'
                    validField = validateHgt(value);
                case 'hcl'
                    validField = validateHcl(value);
                case 'ecl'
                    validField = validateEcl(value);
                case 'pid'
                    validField = validatePid(value);
            end

            if validField
                foundFields = foundFields + 1;
            end
        end
    end
    valid = (foundFields == requiredFields);
end

function valid = validateYear(value, minVal, maxVal)
    year = str2double(value);
    valid = ~isnan(year) && year >= minVal && year <= maxVal;
end

function valid = validateHgt(value)
    if endsWith(value, 'cm')
        hgt = str2double(value(1:end-2));
        valid = ~isnan(hgt) && hgt >= 150 && hgt <= 193;
    elseif endsWith(value, 'in')
        hgt = str2double(value(1:end-2));
        valid = ~isnan(hgt) && hgt >= 59 && hgt <= 76;
    else
        valid = false;
    end
end

function valid = validateHcl(value)
    valid = (length(value) == 7) && (value(1) == '#') && all(ismember(value(2:end), '0123456789abcdef'));
end

function valid = validateEcl(value)
    validEcl = {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'};
    valid = ismember(value, validEcl);
end

function valid = validatePid(value)
    valid = (length(value) == 9) && all(ismember(value, '0123456789'));
end

main();
