
function main()
    reindeers = readReindeerDetails('input.txt');
    reindeers = simulateRaceWithPoints(reindeers, 2503);
    maxPoints = findMaxPoints(reindeers);
    disp(maxPoints);
end

function reindeers = readReindeerDetails(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s %s %s %d %s %s %d %s %s %s %s %s %s %d %s', 'Delimiter', ' ');
    fclose(fid);

    numReindeers = length(lines{1});
    reindeers = struct('speed', cell(1, numReindeers), ...
                       'flyTime', cell(1, numReindeers), ...
                       'restTime', cell(1, numReindeers), ...
                       'distance', cell(1, numReindeers), ...
                       'points', cell(1, numReindeers), ...
                       'flying', cell(1, numReindeers), ...
                       'timeInMode', cell(1, numReindeers));

    for i = 1:numReindeers
        reindeers(i).speed = lines{4}(i);
        reindeers(i).flyTime = lines{7}(i);
        reindeers(i).restTime = lines{14}(i);
        reindeers(i).distance = 0;
        reindeers(i).points = 0;
        reindeers(i).flying = true;
        reindeers(i).timeInMode = 0;
    end
end

function reindeers = simulateRaceWithPoints(reindeers, totalSeconds)
    numReindeers = length(reindeers);
    
    speeds = [reindeers.speed];
    flyTimes = [reindeers.flyTime];
    restTimes = [reindeers.restTime];
    
    distances = zeros(1, numReindeers);
    points = zeros(1, numReindeers);
    flying = true(1, numReindeers);
    timeInMode = zeros(1, numReindeers);

    for t = 1:totalSeconds
        distances = distances + (flying .* speeds);
        
        timeInMode = timeInMode + 1;
        
        switchFlying = (flying & (timeInMode == flyTimes)) | (~flying & (timeInMode == restTimes));
        flying(switchFlying) = ~flying(switchFlying);
        timeInMode(switchFlying) = 0;
        
        maxDistance = max(distances);
        points(distances == maxDistance) = points(distances == maxDistance) + 1;
    end
    
    for i = 1:numReindeers
        reindeers(i).distance = distances(i);
        reindeers(i).points = points(i);
    end
end

function maxPoints = findMaxPoints(reindeers)
    maxPoints = 0;
    for i = 1:length(reindeers)
        if reindeers(i).points > maxPoints
            maxPoints = reindeers(i).points;
        end
    end
end

main();
