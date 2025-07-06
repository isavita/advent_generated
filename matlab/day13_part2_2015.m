
function main()
    happinessMap = readHappinessValues('input.txt');
    happinessMap = addYourself(happinessMap);
    guests = fieldnames(happinessMap);
    maxHappiness = calculateOptimalArrangement(guests, happinessMap);
    disp(maxHappiness);
end

function happinessMap = readHappinessValues(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    happinessMap = struct();
    for i = 1:length(lines)
        parts = strsplit(lines{i});
        from = parts{1};
        to = parts{end}(1:end-1);
        change = str2double(parts{4});
        if strcmp(parts{3}, 'lose')
            change = -change;
        end

        if ~isfield(happinessMap, from)
            happinessMap.(from) = struct();
        end
        happinessMap.(from).(to) = change;
    end
end

function happinessMap = addYourself(happinessMap)
    happinessMap.You = struct();
    guests = fieldnames(happinessMap);
    for i = 1:length(guests)
        guest = guests{i};
        if ~strcmp(guest, 'You')
            happinessMap.(guest).You = 0;
            happinessMap.You.(guest) = 0;
        end
    end
end

function maxHappiness = calculateOptimalArrangement(guests, happinessMap)
    maxHappiness = 0;
    perms = perms(guests);
    for i = 1:size(perms, 1)
        currentHappiness = calculateHappiness(perms(i, :), happinessMap);
        if currentHappiness > maxHappiness
            maxHappiness = currentHappiness;
        end
    end
end

function happiness = calculateHappiness(arrangement, happinessMap)
    happiness = 0;
    n = length(arrangement);
    for i = 1:n
        leftIdx = mod(i - 2 + n, n) + 1;
        rightIdx = mod(i, n) + 1;
        leftGuest = arrangement{leftIdx};
        rightGuest = arrangement{rightIdx};
        currentGuest = arrangement{i};

        happiness = happiness + happinessMap.(currentGuest).(leftGuest);
        happiness = happiness + happinessMap.(currentGuest).(rightGuest);
    end
end

main();
