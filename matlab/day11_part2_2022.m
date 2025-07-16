
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    lines = lines{1};
    fclose(fid);

    monkeys = struct('items', {}, 'op', {}, 'op_val', {}, 'div', {}, 'next', {}, 'inspections', {});
    i = 1;
    monkey_idx = 1;
    while i <= numel(lines)
        monkeys(monkey_idx).items = int64(str2double(regexp(lines{i+1}, '\d+', 'match')));
        
        op_parts = regexp(lines{i+2}, 'new = old (.) (.*)', 'tokens');
        monkeys(monkey_idx).op = op_parts{1}{1};
        monkeys(monkey_idx).op_val = op_parts{1}{2};

        monkeys(monkey_idx).div = str2double(regexp(lines{i+3}, '\d+', 'match'));
        
        true_target = str2double(regexp(lines{i+4}, '\d+', 'match')) + 1;
        false_target = str2double(regexp(lines{i+5}, '\d+', 'match')) + 1;
        monkeys(monkey_idx).next = [false_target, true_target];
        
        monkeys(monkey_idx).inspections = 0;
        
        i = i + 7;
        monkey_idx = monkey_idx + 1;
    end

    total_div = int64(prod([monkeys.div]));
    
    for round = 1:10000
        for m = 1:numel(monkeys)
            num_items = numel(monkeys(m).items);
            monkeys(m).inspections = monkeys(m).inspections + num_items;
            
            for item_idx = 1:num_items
                item = monkeys(m).items(item_idx);
                
                op_val_str = monkeys(m).op_val;
                if strcmp(op_val_str, 'old')
                    operand = item;
                else
                    operand = int64(str2double(op_val_str));
                end
                
                if monkeys(m).op == '+'
                    item = item + operand;
                else
                    item = item * operand;
                end
                
                item = mod(item, total_div);
                
                test_result = mod(item, monkeys(m).div) == 0;
                target_monkey = monkeys(m).next(test_result + 1);
                
                monkeys(target_monkey).items(end+1) = item;
            end
            monkeys(m).items = [];
        end
    end
    
    all_inspections = [monkeys.inspections];
    sorted_inspections = sort(all_inspections, 'descend');
    monkey_business = int64(sorted_inspections(1)) * int64(sorted_inspections(2));
    
    fprintf('%ld\n', monkey_business);
end
