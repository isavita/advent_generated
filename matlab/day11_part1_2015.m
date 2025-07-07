
function main()
    fid = fopen('input.txt', 'r');
    password = fscanf(fid, '%s');
    fclose(fid);

    password = findNextPassword(password);
    disp(password);
end

function nextPassword = findNextPassword(password)
    while true
        password = incrementPassword(password);
        if hasIncreasingStraight(password) && ~containsForbiddenLetters(password) && hasTwoPairs(password)
            nextPassword = password;
            return;
        end
    end
end

function password = incrementPassword(password)
    chars = char(password);
    n = numel(chars);
    i = n;
    while i >= 1
        if chars(i) == 'z'
            chars(i) = 'a';
            i = i - 1;
        else
            chars(i) = char(chars(i) + 1);
            break;
        end
    end
    password = char(chars);
end

function result = hasIncreasingStraight(password)
    n = numel(password);
    result = false;
    for i = 1:(n - 2)
        if password(i+1) == password(i) + 1 && password(i+2) == password(i) + 2
            result = true;
            return;
        end
    end
end

function result = containsForbiddenLetters(password)
    result = ~isempty(strfind(password, 'i')) || ~isempty(strfind(password, 'o')) || ~isempty(strfind(password, 'l'));
end

function result = hasTwoPairs(password)
    pairs = 0;
    i = 1;
    n = numel(password);
    while i <= n - 1
        if password(i) == password(i+1)
            pairs = pairs + 1;
            i = i + 2;
        else
            i = i + 1;
        end
    end
    result = pairs >= 2;
end

main();
