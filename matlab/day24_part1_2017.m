
function main()
    file = fopen('input.txt', 'r');
    components = textscan(file, '%d/%d');
    fclose(file);

    components = [components{1}, components{2}];
    maxStrength = 0;

    function findStrongestBridge(comps, used, port, strength)
        if strength > maxStrength
            maxStrength = strength;
        end

        for i = 1:size(comps, 1)
            if ~used(i) && (comps(i, 1) == port || comps(i, 2) == port)
                used(i) = true;
                nextPort = comps(i, 1);
                if comps(i, 1) == port
                    nextPort = comps(i, 2);
                end
                findStrongestBridge(comps, used, nextPort, strength + sum(comps(i, :)));
                used(i) = false;
            end
        end
    end

    used = false(size(components, 1), 1);
    findStrongestBridge(components, used, 0, 0);
    disp(maxStrength);
end

main();
