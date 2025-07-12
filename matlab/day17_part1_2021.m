
function main()
    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);

    tokens = regexp(line, 'x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)', 'tokens', 'once');
    
    xMin = str2double(tokens{1});
    xMax = str2double(tokens{2});
    yMin = str2double(tokens{3});
    yMax = str2double(tokens{4});

    maxY = -inf;

    xVelMin = 1;
    xVelMax = xMax;

    yVelMin = yMin;
    yVelMax = abs(yMin) - 1;

    for xVel = xVelMin : xVelMax
        for yVel = yVelMin : yVelMax
            xPos = 0;
            yPos = 0;
            curXVel = xVel;
            curYVel = yVel;
            highestY = 0;

            while true
                xPos = xPos + curXVel;
                yPos = yPos + curYVel;

                if yPos > highestY
                    highestY = yPos;
                end

                if xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax
                    if highestY > maxY
                        maxY = highestY;
                    end
                    break;
                end

                isMovingAway = false;
                if (xPos < xMin && curXVel <= 0)
                    isMovingAway = true;
                elseif (xPos > xMax && curXVel >= 0)
                    isMovingAway = true;
                elseif (yPos < yMin && curYVel < 0)
                    isMovingAway = true;
                end

                if isMovingAway
                    break;
                end

                if curXVel > 0
                    curXVel = curXVel - 1;
                elseif curXVel < 0
                    curXVel = curXVel + 1;
                end
                curYVel = curYVel - 1;
            end
        end
    end

    fprintf('%d\n', maxY);
end
