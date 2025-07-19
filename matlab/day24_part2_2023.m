
function main()
    [P, V] = parse_input('input.txt');
    
    ans1 = solve_part1(P, V);
    fprintf('%d\n', ans1);
    
    ans2 = solve_part2(P, V);
    fprintf('%.0f\n', ans2);
end

function [P, V] = parse_input(filename)
    fileID = fopen(filename, 'r');
    data = textscan(fileID, '%d64,%d64,%d64 @ %d64,%d64,%d64');
    fclose(fileID);
    P = [data{1}, data{2}, data{3}];
    V = [data{4}, data{5}, data{6}];
end

function count = solve_part1(P, V)
    min_coord = 200000000000000;
    max_coord = 400000000000000;
    n = size(P, 1);
    count = 0;
    
    Pd = double(P);
    Vd = double(V);

    for i = 1:n
        for j = i + 1:n
            p1 = Pd(i, 1:2);
            v1 = Vd(i, 1:2);
            p2 = Pd(j, 1:2);
            v2 = Vd(j, 1:2);

            A = [v1', -v2'];
            if abs(det(A)) < 1e-9
                continue;
            end
            
            b = (p2 - p1)';
            t = A \ b;

            if all(t > 1e-9)
                ix = p1(1) + v1(1) * t(1);
                iy = p1(2) + v1(2) * t(1);

                if ix >= min_coord && ix <= max_coord && iy >= min_coord && iy <= max_coord
                    count = count + 1;
                end
            end
        end
    end
end

function result = solve_part2(P, V)
    p0 = double(P(1,:)); v0 = double(V(1,:));
    p1 = double(P(2,:)); v1 = double(V(2,:));
    p2 = double(P(3,:)); v2 = double(V(3,:));

    skew = @(v) [0, -v(3), v(2); v(3), 0, -v(1); -v(2), v(1), 0];

    dv1 = v0 - v1;
    dp1 = p0 - p1;
    dv2 = v0 - v2;
    dp2 = p0 - p2;

    M = [skew(dv1), -skew(dp1); 
         skew(dv2), -skew(dp2)];

    b1 = cross(p0, v0) - cross(p1, v1);
    b2 = cross(p0, v0) - cross(p2, v2);
    b = [b1'; b2'];

    solution = M \ b;
    
    result = sum(round(solution(1:3)));
end
