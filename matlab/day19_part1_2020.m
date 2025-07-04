
function main
    fileId = fopen('input.txt', 'r');
    data = fread(fileId, '*char')';
    fclose(fileId);

    parts = strsplit(data, '\n\n');
    rules_str = parts{1};
    messages_str = parts{2};

    rules_dict = containers.Map;
    rule_lines = strsplit(rules_str, '\n');
    for i = 1:length(rule_lines)
        parts = strsplit(rule_lines{i}, ': ');
        rules_dict(parts{1}) = parts{2};
    end

    memo = containers.Map;
    pattern = ['^', get_rule_pattern(rules_dict, '0', memo), '$'];

    messages = strsplit(messages_str, '\n');
    count = 0;
    for i = 1:length(messages)
        if regexp(messages{i}, pattern, 'once')
            count = count + 1;
        end
    end

    disp(count);
end

function pattern = get_rule_pattern(rules_dict, rule_num, memo)
    if isKey(memo, rule_num)
        pattern = memo(rule_num);
        return;
    end

    rule = rules_dict(rule_num);
    if strncmp(rule, '"', 1)
        pattern = rule(2);
    else
        sub_patterns = {};
        parts = strsplit(rule, ' | ');
        for i = 1:length(parts)
            sub_parts = strsplit(parts{i});
            current_sub_pattern = '';
            for j = 1:length(sub_parts)
                current_sub_pattern = [current_sub_pattern, get_rule_pattern(rules_dict, sub_parts{j}, memo)];
            end
            sub_patterns{end+1} = current_sub_pattern;
        end
        pattern = ['(', strjoin(sub_patterns, '|'), ')'];
    end
    memo(rule_num) = pattern;
end
