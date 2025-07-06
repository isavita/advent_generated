
function main()
    claims = readClaims("input.txt");
    fabric = zeros(1000, 1000);

    for i = 1:length(claims)
        claim = claims(i);
        fabric(claim.Y+1:claim.Y+claim.Height, claim.X+1:claim.X+claim.Width) = ...
            fabric(claim.Y+1:claim.Y+claim.Height, claim.X+1:claim.X+claim.Width) + 1;
    end

    for i = 1:length(claims)
        claim = claims(i);
        subFabric = fabric(claim.Y+1:claim.Y+claim.Height, claim.X+1:claim.X+claim.Width);
        if all(subFabric(:) == 1)
            fprintf('%d\n', claim.ID);
            return;
        end
    end
end

function claims = readClaims(filename)
    fid = fopen(filename, 'r');
    claims = struct('ID', {}, 'X', {}, 'Y', {}, 'Width', {}, 'Height', {});
    tline = fgetl(fid);
    while ischar(tline)
        parts = strsplit(tline, ' ');
        id = str2double(parts{1}(2:end));
        coords = strsplit(parts{3}(1:end-1), ',');
        x = str2double(coords{1});
        y = str2double(coords{2});
        dims = strsplit(parts{4}, 'x');
        width = str2double(dims{1});
        height = str2double(dims{2});
        claims(end+1) = struct('ID', id, 'X', x, 'Y', y, 'Width', width, 'Height', height);
        tline = fgetl(fid);
    end
    fclose(fid);
end

main();
