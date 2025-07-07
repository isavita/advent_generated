
function main()
    bots = containers.Map('KeyType', 'double', 'ValueType', 'any');
    outputs = containers.Map('KeyType', 'double', 'ValueType', 'double');
    instructions = {};

    fid = fopen('input.txt', 'r');
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        if isempty(line)
            continue;
        end

        if startsWith(line, 'value')
            [~, tokens] = regexp(line, 'value (\d+) goes to bot (\d+)', 'match', 'tokens');
            value = str2double(tokens{1}{1});
            botNum = str2double(tokens{1}{2});

            if ~isKey(bots, botNum)
                bots(botNum) = struct('low', [], 'high', [], 'lowTarget', '', 'highTarget', '', 'lowTargetType', '', 'highTargetType', '');
            end

            bot = bots(botNum);
            if isempty(bot.low) || value < bot.low
                bot.high = bot.low;
                bot.low = value;
            else
                bot.high = value;
            end
            bots(botNum) = bot;
            processBot(botNum, bots, outputs);
        else
            instructions{end+1} = line;
        end
    end
    fclose(fid);

    for i = 1:length(instructions)
        line = instructions{i};
        [~, tokens] = regexp(line, 'bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)', 'match', 'tokens');
        botNum = str2double(tokens{1}{1});
        lowTargetType = tokens{1}{2};
        lowTarget = str2double(tokens{1}{3});
        highTargetType = tokens{1}{4};
        highTarget = str2double(tokens{1}{5});

        if ~isKey(bots, botNum)
            bots(botNum) = struct('low', [], 'high', [], 'lowTarget', '', 'highTarget', '', 'lowTargetType', '', 'highTargetType', '');
        end

        bot = bots(botNum);
        bot.lowTarget = lowTarget;
        bot.highTarget = highTarget;
        bot.lowTargetType = lowTargetType;
        bot.highTargetType = highTargetType;
        bots(botNum) = bot;
        processBot(botNum, bots, outputs);
    end

    botNumber = findBotComparingValues(61, 17, bots);
    if ~isempty(botNumber)
        fprintf('The bot responsible for comparing value-61 microchips with value-17 microchips is bot %d.\n', botNumber);
    else
        fprintf('No bot found comparing value-61 microchips with value-17 microchips.\n');
    end

    if isKey(outputs, 0) && isKey(outputs, 1) && isKey(outputs, 2)
        outputProduct = outputs(0) * outputs(1) * outputs(2);
        fprintf('The product of the values in outputs 0, 1, and 2 is %d.\n', outputProduct);
    else
        fprintf('Not all required outputs (0, 1, 2) have values.\n');
    end
end

function processBot(botNum, bots, outputs)
    if isKey(bots, botNum)
        bot = bots(botNum);
        if ~isempty(bot.low) && ~isempty(bot.high) && ~isempty(bot.lowTarget) && ~isempty(bot.highTarget)
            if strcmp(bot.lowTargetType, 'bot')
                giveValueToBot(bot.lowTarget, bot.low, bots, outputs);
            elseif strcmp(bot.lowTargetType, 'output')
                outputs(bot.lowTarget) = bot.low;
            end

            if strcmp(bot.highTargetType, 'bot')
                giveValueToBot(bot.highTarget, bot.high, bots, outputs);
            elseif strcmp(bot.highTargetType, 'output')
                outputs(bot.highTarget) = bot.high;
            end

            bot.low = [];
            bot.high = [];
            bots(botNum) = bot;
        end
    end
end

function giveValueToBot(botNum, value, bots, outputs)
    if ~isKey(bots, botNum)
        bots(botNum) = struct('low', [], 'high', [], 'lowTarget', '', 'highTarget', '', 'lowTargetType', '', 'highTargetType', '');
    end

    bot = bots(botNum);
    if isempty(bot.low) || value < bot.low
        bot.high = bot.low;
        bot.low = value;
    else
        bot.high = value;
    end
    bots(botNum) = bot;
    processBot(botNum, bots, outputs);
end

function botNumber = findBotComparingValues(value1, value2, bots)
    botNumber = [];
    botKeys = keys(bots);
    for i = 1:length(botKeys)
        currentBotNum = botKeys{i};
        bot = bots(currentBotNum);
        if (~isempty(bot.low) && ~isempty(bot.high)) && ...
           ((bot.low == value1 && bot.high == value2) || (bot.low == value2 && bot.high == value1))
            botNumber = currentBotNum;
            return;
        end
    end
end

main();
