
function main()
    fileId = fopen('input.txt', 'r');
    moons = zeros(3, 3);
    line = fgetl(fileId);
    i = 1;
    while ischar(line)
        coords = regexp(line, '<x=(-?\d+), y=(-?\d+), z=(-?\d+)>', 'tokens');
        moons(i, :) = [str2double(coords{1}{1}), str2double(coords{1}{2}), str2double(coords{1}{3})];
        line = fgetl(fileId);
        i = i + 1;
    end
    fclose(fileId);

    velocities = zeros(size(moons));

    for step = 1:1000
        for i = 1:size(moons, 1)
            for j = i+1:size(moons, 1)
                for axis = 1:3
                    if moons(i, axis) < moons(j, axis)
                        velocities(i, axis) = velocities(i, axis) + 1;
                        velocities(j, axis) = velocities(j, axis) - 1;
                    elseif moons(i, axis) > moons(j, axis)
                        velocities(i, axis) = velocities(i, axis) - 1;
                        velocities(j, axis) = velocities(j, axis) + 1;
                    end
                end
            end
        end
        moons = moons + velocities;
    end

    potential = sum(abs(moons), 2);
    kinetic = sum(abs(velocities), 2);
    total_energy = sum(potential .* kinetic);

    disp(total_energy);
end

main();
