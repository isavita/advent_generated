
function main()
    fid = fopen('input.txt', 'r');
    moves = strsplit(fscanf(fid, '%s'), ',');
    fclose(fid);

    programs = 'abcdefghijklmnop';

    for move = moves
        type = move{1}(1);
        args = move{1}(2:end);

        switch type
            case 's'
                X = str2double(args);
                programs = [programs(end-X+1:end), programs(1:end-X)];
            case 'x'
                indices = str2double(strsplit(args, '/'));
                A = indices(1);
                B = indices(2);
                temp = programs(A+1);
                programs(A+1) = programs(B+1);
                programs(B+1) = temp;
            case 'p'
                partners = strsplit(args, '/');
                A = partners{1};
                B = partners{2};
                idxA = find(programs == A, 1);
                idxB = find(programs == B, 1);
                temp = programs(idxA);
                programs(idxA) = programs(idxB);
                programs(idxB) = temp;
        end
    end

    disp(programs);
end

main();
