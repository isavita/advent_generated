
function main()
    filename = 'input.txt';
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file %s', filename);
    end

    people_map = containers.Map;
    person_count = 0;
    raw_happiness_entries = {};

    while ~feof(fid)
        line = fgetl(fid);
        if ischar(line)
            parts = strsplit(line, ' ');
            person = parts{1};
            neighbor = parts{end};
            neighbor = neighbor(1:end-1);

            happiness_change = str2double(parts{4});
            if strcmp(parts{3}, 'lose')
                happiness_change = -happiness_change;
            end

            if ~isKey(people_map, person)
                person_count = person_count + 1;
                people_map(person) = person_count;
            end
            if ~isKey(people_map, neighbor)
                person_count = person_count + 1;
                people_map(neighbor) = person_count;
            end
            raw_happiness_entries{end+1} = {person, neighbor, happiness_change};
        end
    end
    fclose(fid);

    num_people = person_count;
    happiness_matrix = zeros(num_people, num_people);

    for i = 1:length(raw_happiness_entries)
        entry = raw_happiness_entries{i};
        person_idx = people_map(entry{1});
        neighbor_idx = people_map(entry{2});
        happiness_matrix(person_idx, neighbor_idx) = entry{3};
    end

    all_people_indices = 1:num_people;
    seating_permutations = perms(all_people_indices);

    optimal_happiness = -inf;

    for i = 1:size(seating_permutations, 1)
        current_seating_indices = seating_permutations(i, :);
        total_change = calculate_happiness_change(current_seating_indices, happiness_matrix);
        optimal_happiness = max(optimal_happiness, total_change);
    end

    fprintf('%.0f\n', optimal_happiness);
end

function total_change = calculate_happiness_change(seating_indices, happiness_matrix)
    total_change = 0;
    N = length(seating_indices);

    for i = 1:N
        person_idx = seating_indices(i);
        left_neighbor_idx = seating_indices(mod(i - 2 + N, N) + 1);
        right_neighbor_idx = seating_indices(mod(i, N) + 1);

        total_change = total_change + happiness_matrix(person_idx, left_neighbor_idx);
        total_change = total_change + happiness_matrix(person_idx, right_neighbor_idx);
    end
end
