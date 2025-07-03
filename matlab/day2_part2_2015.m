
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%dx%dx%d', 'Delimiter', 'x');
    fclose(fid);

    l = lines{1};
    w = lines{2};
    h = lines{3};

    sides1 = l .* w;
    sides2 = w .* h;
    sides3 = h .* l;

    min_sides = min([sides1, sides2, sides3], [], 2);
    total_paper = sum(2 * (sides1 + sides2 + sides3) + min_sides);

    perimeter_sides = [l + w, w + h, h + l];
    min_perimeter = min(perimeter_sides, [], 2);
    total_ribbon = sum(2 * min_perimeter + l .* w .* h);

    fprintf('%d\n', total_paper);
    fprintf('%d\n', total_ribbon);
end
