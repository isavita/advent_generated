
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.Read (readMaybe)

data Group = Group 
    { gid :: Int, army :: String, units :: Int, hp :: Int, dmg :: Int, 
      dmgType :: String, initiative :: Int, weak :: [String], immune :: [String] } 
    deriving (Show, Eq)

power :: Group -> Int
power g = units g * dmg g

calcDmg :: Group -> Group -> Int
calcDmg att def
    | dmgType att `elem` immune def = 0
    | dmgType att `elem` weak def = 2 * power att
    | otherwise = power att

parseGroup :: Int -> String -> String -> Group
parseGroup i a s = 
    let ws = words $ map (\c -> if c `elem` "(),;" then ' ' else c) s
        u = read $ ws !! 0
        h = read $ ws !! 4
        ini = read $ last ws
        d = read $ ws !! (length ws - 6)
        dt = ws !! (length ws - 5)
        mods = takeWhile (/= "with") $ drop 7 ws
        (imms, weaks) = parseMods mods [] [] ""
    in Group i a u h d dt ini weaks imms

parseMods [] i w _ = (i, w)
parseMods (x:xs) i w mode
    | x == "immune" = parseMods xs i w "i"
    | x == "weak" = parseMods xs i w "w"
    | x == "to" = parseMods xs i w mode
    | mode == "i" = parseMods xs (x:i) w mode
    | mode == "w" = parseMods xs i (x:w) mode
    | otherwise = parseMods xs i w mode

simulate :: M.Map Int Group -> Int
simulate groups
    | null (filter ((== "Immune System") . army) gs) || null (filter ((== "Infection") . army) gs) = sum (map units gs)
    | otherwise = 
        let targets = selectTargets gs []
            nextGroups = attack (sortOn (negate . initiative) gs) targets groups
            gs' = filter ((>0) . units) (M.elems nextGroups)
        in if gs == gs' then 0 else simulate (M.fromList [(gid g, g) | g <- gs'])
    where 
        gs = sortOn (\g -> (negate $ power g, negate $ initiative g)) (M.elems groups)
        selectTargets [] _ = []
        selectTargets (att:as) picked = 
            let defs = filter (\d -> army d /= army att && gid d `notElem` picked && calcDmg att d > 0) gs
                best = listToMaybe $ sortOn (\d -> (negate $ calcDmg att d, negate $ power d, negate $ initiative d)) defs
            in case best of
                Just b -> (gid att, gid b) : selectTargets as (gid b : picked)
                Nothing -> selectTargets as picked

attack [] _ m = m
attack (att:as) targets m =
    let att' = m M.! gid att
        targetId = lookup (gid att) targets
    in if units att' <= 0 || isNothing targetId then attack as targets m
       else let def = m M.! fromJust targetId
                dmgDealt = calcDmg att' def
                killed = min (units def) (dmgDealt `div` hp def)
                newDef = def { units = units def - killed }
            in attack as targets (M.insert (gid def) newDef m)

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
        parse _ _ [] = []
        parse a i (l:xs)
            | "Immune System:" `isInfixOf` l = parse "Immune System" i xs
            | "Infection:" `isInfixOf` l = parse "Infection" i xs
            | null l = parse a i xs
            | otherwise = parseGroup i a l : parse a (i+1) xs
        gs = parse "" 1 ls
        m = M.fromList [(gid g, g) | g <- gs]
    print $ simulate m
