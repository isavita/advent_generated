
function main()
    fid = fopen('input.txt', 'r');
    moves = strsplit(fgetl(fid), ',');
    fclose(fid);

    programs = 'abcdefghijklmnop';
    initial = programs;
    cycleLen = 0;

    for i = 1:1e9
        for j = 1:length(moves)
            move = moves{j};
            switch move(1)
                case 's'
                    x = str2double(move(2:end));
                    programs = spin(programs, x);
                case 'x'
                    positions = strsplit(move(2:end), '/');
                    A = str2double(positions{1});
                    B = str2double(positions{2});
                    programs = exchange(programs, A + 1, B + 1);
                case 'p'
                    positions = strsplit(move(2:end), '/');
                    A = positions{1}(1);
                    B = positions{2}(1);
                    programs = partner(programs, A, B);
            end
        end

        if strcmp(programs, initial)
            cycleLen = i;
            break;
        end
    end

    programs = initial;
    num_iterations = 1e9;
    remaining_iterations = mod(num_iterations, cycleLen);

    for i = 1:remaining_iterations
        for j = 1:length(moves)
            move = moves{j};
            switch move(1)
                case 's'
                    x = str2double(move(2:end));
                    programs = spin(programs, x);
                case 'x'
                    positions = strsplit(move(2:end), '/');
                    A = str2double(positions{1});
                    B = str2double(positions{2});
                    programs = exchange(programs, A + 1, B + 1);
                case 'p'
                    positions = strsplit(move(2:end), '/');
                    A = positions{1}(1);
                    B = positions{2}(1);
                    programs = partner(programs, A, B);
            end
        end
    end

    disp(programs);
end

function programs = spin(programs, x)
    n = length(programs);
    programs = [programs(n-x+1:end), programs(1:n-x)];
end

function programs = exchange(programs, A, B)
    temp = programs(A);
    programs(A) = programs(B);
    programs(B) = temp;
end

function programs = partner(programs, A, B)
    indexA = find(programs == A, 1);
    indexB = find(programs == B, 1);
    programs = exchange(programs, indexA, indexB);
end

main();
