
function readAsteroids(filename)
    asteroids = []
    open(filename) do file
        for line in eachline(file)
            asteroidRow = [char == '#' for char in line]
            push!(asteroids, asteroidRow)
        end
    end
    return asteroids
end

function findBestAsteroidLocation(asteroids)
    maxCount = 0
    for (y, row) in enumerate(asteroids)
        for (x, isAsteroid) in enumerate(row)
            if isAsteroid
                count = countVisibleAsteroids(asteroids, x, y)
                if count > maxCount
                    maxCount = count
                end
            end
        end
    end
    return maxCount
end

function countVisibleAsteroids(asteroids, x, y)
    angles = Dict{Float64, Bool}()
    for (otherY, row) in enumerate(asteroids)
        for (otherX, isAsteroid) in enumerate(row)
            if isAsteroid && !(otherX == x && otherY == y)
                angle = atan(otherY-y, otherX-x) # Fixed atan2 to atan
                angles[angle] = true
            end
        end
    end
    return length(angles)
end

asteroids = readAsteroids("input.txt")
maxCount = findBestAsteroidLocation(asteroids)
println(maxCount)
