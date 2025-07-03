
function main()
    fid = fopen("input.txt", "r");
    directions = strsplit(fscanf(fid, "%s"), ",");
    fclose(fid);

    dx = 0;
    dy = 0;
    max_dist = 0;

    for i = 1:length(directions)
        direction = directions{i};
        switch direction
            case "n"
                dy = dy + 1;
            case "s"
                dy = dy - 1;
            case "ne"
                dx = dx + 1;
            case "sw"
                dx = dx - 1;
            case "nw"
                dx = dx - 1;
                dy = dy + 1;
            case "se"
                dx = dx + 1;
                dy = dy - 1;
        end
        max_dist = max(max_dist, max(abs(dx), abs(dy)));
    end

    fprintf("%d\n", max(abs(dx), abs(dy)));
    fprintf("%d\n", max_dist);
end

main();
