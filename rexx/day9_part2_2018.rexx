
/* Rexx */
call main
exit

main:
    numeric digits 20
    call readInput 'input.txt'
    lastMarble = lastMarble * 100
    say playMarbleGame(players, lastMarble)
return

readInput:
    parse arg filename
    line = linein(filename)
    call stream filename, 'C', 'CLOSE'
    parse var line players 'players; last marble is worth' lastMarble 'points'
return

playMarbleGame: procedure
    parse arg players, lastMarble
    scores. = 0
    current = 0
    next.0 = 0
    prev.0 = 0

    do marble = 1 to lastMarble
        if marble // 23 = 0 then do
            player = marble // players
            to_remove = current
            do 7
                to_remove = prev.to_remove
            end
            scores.player = scores.player + marble + to_remove
            p = prev.to_remove
            n = next.to_remove
            next.p = n
            prev.n = p
            current = n
        end
        else do
            p = next.current
            n = next.p
            new_marble = marble
            next.p = new_marble
            prev.new_marble = p
            next.new_marble = n
            prev.n = new_marble
            current = new_marble
        end
    end

    maxScore = 0
    do i = 0 for players
        maxScore = max(maxScore, scores.i)
    end
return maxScore
