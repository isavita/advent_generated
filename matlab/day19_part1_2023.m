
function solve()
    filename = 'input.txt';
    [workflowsMap, parts] = parse_input(filename);

    total_sum = 0;
    start_workflow_name = 'in';

    if ~isKey(workflowsMap, start_workflow_name)
        error('Error: ''in'' workflow not found.');
    end

    for i = 1:length(parts)
        current_part = parts(i);
        if evaluate_part(current_part, start_workflow_name, workflowsMap)
            total_sum = total_sum + current_part.x + current_part.m + current_part.a + current_part.s;
        end
    end

    fprintf('%d\n', total_sum);
end

function [workflowsMap, parts] = parse_input(filename)
    workflowsMap = containers.Map;
    parts = struct('x', {}, 'm', {}, 'a', {}, 's', {});

    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    parsing_workflows = true;
    part_idx = 0;

    while ~feof(fid)
        line = fgetl(fid);
        if ~ischar(line), break; end

        if isempty(line)
            parsing_workflows = false;
            continue;
        end

        if parsing_workflows
            brace_idx = strfind(line, '{');
            wf_name = line(1:brace_idx-1);
            rules_str = line(brace_idx+1:end-1);

            wf_rules_str = strsplit(rules_str, ',');
            current_wf_rules = struct('category', {}, 'op', {}, 'value', {}, 'dest_name', {});

            for i = 1:length(wf_rules_str)
                rule_str = wf_rules_str{i};
                rule_struct = parse_rule_matlab(rule_str);
                current_wf_rules(i) = rule_struct;
            end

            workflow_struct.name = wf_name;
            workflow_struct.rules = current_wf_rules;
            workflowsMap(wf_name) = workflow_struct;

        else
            part_idx = part_idx + 1;
            vals = sscanf(line, '{x=%d,m=%d,a=%d,s=%d}');
            parts(part_idx).x = vals(1);
            parts(part_idx).m = vals(2);
            parts(part_idx).a = vals(3);
            parts(part_idx).s = vals(4);
        end
    end
    fclose(fid);
end

function rule_struct = parse_rule_matlab(rule_str)
    colon_idx = strfind(rule_str, ':');
    if isempty(colon_idx)
        rule_struct.category = '';
        rule_struct.op = '';
        rule_struct.value = 0;
        rule_struct.dest_name = rule_str;
    else
        condition_str = rule_str(1:colon_idx-1);
        rule_struct.dest_name = rule_str(colon_idx+1:end);

        rule_struct.category = condition_str(1);
        rule_struct.op = condition_str(2);
        rule_struct.value = str2double(condition_str(3:end));
    end
end

function is_accepted = evaluate_part(part, start_workflow_name, workflowsMap)
    current_wf_name = start_workflow_name;

    while true
        if strcmp(current_wf_name, 'A')
            is_accepted = true;
            return;
        elseif strcmp(current_wf_name, 'R')
            is_accepted = false;
            return;
        end

        current_wf = workflowsMap(current_wf_name);

        rule_matched = false;
        for i = 1:length(current_wf.rules)
            rule = current_wf.rules(i);
            condition_met = false;

            if isempty(rule.category)
                condition_met = true;
            else
                part_value = 0;
                switch rule.category
                    case 'x', part_value = part.x;
                    case 'm', part_value = part.m;
                    case 'a', part_value = part.a;
                    case 's', part_value = part.s;
                end

                if rule.op == '<'
                    condition_met = part_value < rule.value;
                elseif rule.op == '>'
                    condition_met = part_value > rule.value;
                end
            end

            if condition_met
                current_wf_name = rule.dest_name;
                rule_matched = true;
                break;
            end
        end

        if ~rule_matched
            is_accepted = false;
            return;
        end
    end
end
