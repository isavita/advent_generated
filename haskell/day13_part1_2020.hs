
main = do
    contents <- readFile "input.txt"
    let [earliestDeparture, busIDs] = lines contents
        earliestDeparture' = read earliestDeparture :: Int
        busIDs' = map (\x -> if x == "x" then 0 else read x :: Int) $ words $ map (\x -> if x == ',' then ' ' else x) busIDs
        validBusIDs = filter (/= 0) busIDs'
        (earliestBusID, minWaitTime) = foldl (\(earliestBusID, minWaitTime) busID -> let waitTime = busID - (earliestDeparture' `mod` busID) in if waitTime < minWaitTime then (busID, waitTime) else (earliestBusID, minWaitTime)) (0, earliestDeparture') validBusIDs
    print $ earliestBusID * minWaitTime
