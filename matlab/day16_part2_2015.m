
function main()
    % Define the target properties based on the MFCSAM output
    targetProperties = containers.Map;
    targetProperties('children') = 3;
    targetProperties('cats') = 7;
    targetProperties('samoyeds') = 2;
    targetProperties('pomeranians') = 3;
    targetProperties('akitas') = 0;
    targetProperties('vizslas') = 0;
    targetProperties('goldfish') = 5;
    targetProperties('trees') = 3;
    targetProperties('cars') = 2;
    targetProperties('perfumes') = 1;

    % Define the range-based properties for Part Two
    greaterThanProperties = {'cats', 'trees'};
    lessThanProperties = {'pomeranians', 'goldfish'};

    % Read input from input.txt
    auntsData = readInput('input.txt');

    % --- Part One ---
    fprintf('--- Part One ---\n');
    for i = 1:length(auntsData)
        if isMatchPartOne(auntsData{i}, targetProperties)
            fprintf('Aunt Sue #%d matches the criteria for Part One.\n', i);
            break; % Assuming only one match for Part One
        end
    end

    % --- Part Two ---
    fprintf('\n--- Part Two ---\n');
    for i = 1:length(auntsData)
        if isMatchPartTwo(auntsData{i}, targetProperties, greaterThanProperties, lessThanProperties)
            fprintf('Aunt Sue #%d matches the criteria for Part Two.\n', i);
            break; % Assuming only one match for Part Two
        end
    end
end

function auntsData = readInput(filename)
    % Reads the input file and parses each line into a map of properties.
    auntsData = {};
    try
        fid = fopen(filename, 'r');
        if fid == -1
            error('Could not open file: %s', filename);
        end

        line = fgetl(fid);
        while ischar(line)
            properties = containers.Map;
            % Remove "Sue #: " prefix
            line = strrep(line, 'Sue #: ', '');

            % Split the line by commas
            pairs = strsplit(line, ', ');

            for j = 1:length(pairs)
                parts = strsplit(pairs{j}, ': ');
                if length(parts) == 2
                    propertyName = parts{1};
                    propertyValue = str2double(parts{2});
                    properties(propertyName) = propertyValue;
                end
            end
            auntsData{end+1} = properties;
            line = fgetl(fid);
        end
        fclose(fid);
    catch ME
        fprintf('Error reading file: %s\n', ME.message);
        if fid ~= -1
            fclose(fid);
        end
        rethrow(ME);
    end
end

function isMatch = isMatchPartOne(auntProperties, targetProperties)
    % Checks if an aunt's properties match the target properties for Part One.
    isMatch = true;
    targetKeys = targetProperties.keys;

    for k = 1:length(targetKeys)
        key = targetKeys{k};
        if auntProperties.isKey(key)
            if auntProperties(key) ~= targetProperties(key)
                isMatch = false;
                break;
            end
        end
    end
end

function isMatch = isMatchPartTwo(auntProperties, targetProperties, greaterThanProps, lessThanProps)
    % Checks if an aunt's properties match the target properties for Part Two,
    % considering range-based comparisons.
    isMatch = true;
    targetKeys = targetProperties.keys;

    for k = 1:length(targetKeys)
        key = targetKeys{k};
        if auntProperties.isKey(key)
            value = auntProperties(key);
            targetValue = targetProperties(key);

            if ismember(key, greaterThanProps)
                if value <= targetValue
                    isMatch = false;
                    break;
                end
            elseif ismember(key, lessThanProps)
                if value >= targetValue
                    isMatch = false;
                    break;
                end
            else % Exact match for other properties
                if value ~= targetValue
                    isMatch = false;
                    break;
                end
            end
        end
    end
end

% To run this program, save it as a .m file (e.g., solve_aunt_sue.m)
% and then call the main function from the MATLAB command window:
% solve_aunt_sue()
%
% Ensure you have an input.txt file in the same directory with the data.
% Example input.txt content:
% Sue 1: cars: 9, goldfish: 7, trees: 3
% Sue 2: akitas: 0, vizslas: 0, goldfish: 5
% Sue 3: pomeranians: 3, cats: 7, trees: 3
% ... (and so on for 500 aunts)
