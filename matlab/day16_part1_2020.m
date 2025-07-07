
function main()
    fid = fopen('input.txt', 'r');
    inputStr = fread(fid, '*char')';
    fclose(fid);

    sections = strsplit(inputStr, '\n\n');

    rules = parseRules(sections{1});
    myTicket = parseTicket(sections{2});
    nearbyTickets = parseNearbyTickets(sections{3});

    errorRate = calculateErrorRate(nearbyTickets, rules);
    disp(errorRate);
end

function rules = parseRules(ruleSection)
    ruleLines = strsplit(ruleSection, '\n');
    numRules = numel(ruleLines);
    rules = cell(numRules, 1);
    for i = 1:numRules
        parts = strsplit(ruleLines{i}, ': ');
        name = parts{1};
        rangeParts = strsplit(parts{2}, ' or ');
        ranges = zeros(numel(rangeParts), 2);
        for j = 1:numel(rangeParts)
            rangeVals = str2double(strsplit(rangeParts{j}, '-'));
            ranges(j, :) = rangeVals;
        end
        rules{i} = struct('name', name, 'ranges', ranges);
    end
end

function ticket = parseTicket(ticketSection)
    ticket = str2double(strsplit(strsplit(ticketSection, '\n'){2}, ','));
end

function nearbyTickets = parseNearbyTickets(ticketSection)
    ticketLines = strsplit(ticketSection, '\n');
    ticketLines = ticketLines(2:end);
    numTickets = numel(ticketLines);
    nearbyTickets = zeros(numTickets, numel(strsplit(ticketLines{1}, ',')));
    for i = 1:numTickets
        nearbyTickets(i, :) = str2double(strsplit(ticketLines{i}, ','));
    end
end

function isValid = isValidValue(value, rules)
    isValid = false;
    for i = 1:numel(rules)
        ruleRanges = rules{i}.ranges;
        if any(value >= ruleRanges(:, 1) & value <= ruleRanges(:, 2))
            isValid = true;
            return;
        end
    end
end

function errorRate = calculateErrorRate(tickets, rules)
    errorRate = 0;
    for i = 1:size(tickets, 1)
        for j = 1:size(tickets, 2)
            if ~isValidValue(tickets(i, j), rules)
                errorRate = errorRate + tickets(i, j);
            end
        end
    end
end

main();
