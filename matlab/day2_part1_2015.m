
function main()
    fid = fopen('input.txt', 'r');
    total_paper = 0;
    while ~feof(fid)
        line = fgetl(fid);
        dims = sscanf(line, '%dx%dx%d');
        l = dims(1);
        w = dims(2);
        h = dims(3);
        sides = [l*w, w*h, h*l];
        total_paper = total_paper + 2*sum(sides) + min(sides);
    end
    fclose(fid);
    fprintf('%d\n', total_paper);
end

main();
