
function main()
    fileId = fopen('input.txt', 'r');
    if fileId == -1
        error('Error opening file');
    end

    cubeGrid = zeros(101, 101, 101, 'int8');
    steps = struct('action', {}, 'xStart', {}, 'xEnd', {}, 'yStart', {}, 'yEnd', {}, 'zStart', {}, 'zEnd', {});
    stepsCount = 0;

    while ~feof(fileId)
        line = strtrim(fgets(fileId));
        if isempty(line)
            continue;
        end

        parts = strsplit(line, ' ');
        action = parts{1};
        coords = strsplit(parts{2}, ',');

        xRange = str2double(strsplit(strrep(coords{1}, 'x=', ''), '..'));
        yRange = str2double(strsplit(strrep(coords{2}, 'y=', ''), '..'));
        zRange = str2double(strsplit(strrep(coords{3}, 'z=', ''), '..'));

        stepsCount = stepsCount + 1;
        steps(stepsCount).action = action;
        steps(stepsCount).xStart = xRange(1);
        steps(stepsCount).xEnd = xRange(2);
        steps(stepsCount).yStart = yRange(1);
        steps(stepsCount).yEnd = yRange(2);
        steps(stepsCount).zStart = zRange(1);
        steps(stepsCount).zEnd = zRange(2);
    end

    fclose(fileId);

    minCoord = -50;

    for i = 1:stepsCount
        step = steps(i);
        if step.xStart < minCoord || step.xEnd > 50 || step.yStart < minCoord || step.yEnd > 50 || step.zStart < minCoord || step.zEnd > 50
            continue;
        end

        xIdx = (step.xStart - minCoord + 1):(step.xEnd - minCoord + 1);
        yIdx = (step.yStart - minCoord + 1):(step.yEnd - minCoord + 1);
        zIdx = (step.zStart - minCoord + 1):(step.zEnd - minCoord + 1);

        if strcmp(step.action, 'on')
            cubeGrid(xIdx, yIdx, zIdx) = 1;
        else
            cubeGrid(xIdx, yIdx, zIdx) = 0;
        end
    end

    onCubes = sum(cubeGrid(:));
    fprintf('%d\n', onCubes);
end

