
import System.IO
import qualified Data.Set as S

type Point = (Int, Int)
type Rock = [Point]
type Chamber = S.Set Point

rockShapes :: [Rock]
rockShapes =
  [ [(0,0), (1,0), (2,0), (3,0)]
  , [(1,0), (0,1), (1,1), (2,1), (1,2)]
  , [(0,0), (1,0), (2,0), (2,1), (2,2)]
  , [(0,0), (0,1), (0,2), (0,3)]
  , [(0,0), (1,0), (0,1), (1,1)]
  ]

canMove :: Rock -> Char -> Chamber -> Bool -> Rock
canMove rock direction chamber highestY =
  let
    movedRock = map movePoint rock
    movePoint (x, y) =
      case direction of
        '<' -> (x - 1, y)
        '>' -> (x + 1, y)
        'v' -> (x, y - 1)
        _   -> error "Invalid direction"
    isWithinBounds (x, y) = x >= 0 && x <= 6 && y >= 1
    noCollision = all (`S.notMember` chamber) movedRock
    inBounds = all isWithinBounds movedRock
  in
    if inBounds && noCollision then movedRock else rock

simulate :: String -> Int -> Int
simulate jetPattern totalRocks =
  let
    initialChamber = S.fromList [(x, 0) | x <- [0..6]]
    initialHighestY = 0
    initialJetIndex = 0

    simulateRock :: (Chamber, Int, Int) -> Rock -> (Chamber, Int, Int)
    simulateRock (chamber, highestY, jetIndex) rockShape =
      let
        initialRock = map (\(dx, dy) -> (2 + dx, highestY + 4 + dy)) rockShape
        (finalRock, finalJetIndex) = dropRock chamber highestY jetIndex initialRock

        dropRock :: Chamber -> Int -> Int -> Rock -> (Rock, Int)
        dropRock chamber highestY jetIndex rock =
          let
            jetDir = jetPattern !! (jetIndex `mod` length jetPattern)
            rockAfterJet = canMove rock jetDir chamber False
            nextJetIndex = jetIndex + 1
            rockAfterDown = canMove rockAfterJet 'v' chamber False

            in
              if rockAfterDown == rockAfterJet then
                (rockAfterJet, nextJetIndex)
              else
                dropRock chamber highestY nextJetIndex rockAfterDown

        updatedChamber = S.union chamber (S.fromList finalRock)
        updatedHighestY = maximum (map snd (S.toList updatedChamber))

      in
        (updatedChamber, updatedHighestY, finalJetIndex)


    finalState = foldl (\(chamber, highestY, jetIndex) rockNumber ->
                         simulateRock (chamber, highestY, jetIndex) (rockShapes !! (rockNumber `mod` length rockShapes)))
                       (initialChamber, initialHighestY, initialJetIndex)
                       [0..(totalRocks - 1)]

  in
    let (_, finalHighestY, _) = finalState in finalHighestY

main :: IO ()
main = do
  jetPattern <- readFile "input.txt"
  let totalRocks = 2022
  let finalHeight = simulate jetPattern totalRocks
  print finalHeight
