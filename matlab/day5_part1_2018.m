
function main()
    fid = fopen('input.txt', 'r');
    polymer = fscanf(fid, '%s');
    fclose(fid);

    result = react(polymer);
    fprintf('%d\n', length(result));
end

function reacted_polymer = react(polymer)
    while true
        found_reaction = false;
        new_polymer = '';
        i = 1;
        while i <= length(polymer)
            if i < length(polymer) && ...
               ( (polymer(i) ~= polymer(i+1)) && ...
                 (abs(polymer(i) - polymer(i+1)) == 32) )
                i = i + 2;
                found_reaction = true;
            else
                new_polymer = [new_polymer, polymer(i)];
                i = i + 1;
            end
        end
        polymer = new_polymer;
        if ~found_reaction
            break;
        end
    end
    reacted_polymer = polymer;
end

main();
