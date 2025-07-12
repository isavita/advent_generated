
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    C = textscan(fid, 'p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>');
    fclose(fid);

    ax = C{7};
    ay = C{8};
    az = C{9};

    manhattan_distances = abs(ax) + abs(ay) + abs(az);

    [~, closest_particle_idx] = min(manhattan_distances);

    fprintf('%d\n', closest_particle_idx - 1);
end
