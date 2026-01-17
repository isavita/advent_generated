
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (Down(..))
import Control.Monad (guard)
import Data.Char (isDigit)

data Group = Group {
    gid :: Int, army :: Int, units :: Int, hp :: Int,
    atk :: Int, atkType :: String, ini :: Int,
    immunes :: [String], weaks :: [String]
} deriving (Show, Eq)

effectivePower g = units g * atk g

calcDamage a t
    | atkType a `elem` immunes t = 0
    | atkType a `elem` weaks t = 2 * effectivePower a
    | otherwise = effectivePower a

splitBy c s = case dropWhile (== c) s of
    "" -> []
    s' -> let (w, s'') = break (== c) s' in w : splitBy c s''

trim = let f = dropWhile (== ' ') . reverse in f . f

parseTraits s
    | '(' `notElem` s = ([], [])
    | otherwise =
        let content = takeWhile (/= ')') (tail (dropWhile (/= '(') s))
            parts = map trim (splitBy ';' content)
            f p | "immune to " `isPrefixOf` p = (map trim (splitBy ',' (drop 10 p)), [])
                | "weak to " `isPrefixOf` p = ([], map trim (splitBy ',' (drop 8 p)))
                | otherwise = ([], [])
            (is, ws) = unzip (map f parts)
        in (concat is, concat ws)

parseGroup aId gId s =
    let ws = words s
        un = read (ws !! 0)
        hitp = read (ws !! 4)
        i = read (last ws)
        atkd = read (ws !! (length ws - 6))
        atkt = ws !! (length ws - 5)
        (imm, wk) = parseTraits s
    in Group gId aId un hitp atkd atkt i imm wk

selectTargets gs = snd $ foldl' findT ([], []) (sortOn (\g -> (Down (effectivePower g), Down (ini g))) gs)
  where
    findT (targeted, pairs) a =
        let enemies = filter (\e -> army e /= army a && units e > 0 && not (gid e `elem` targeted)) gs
            best = listToMaybe $ sortOn (\e -> (Down (calcDamage a e), Down (effectivePower e), Down (ini e)))
                   [e | e <- enemies, calcDamage a e > 0]
        in case best of
            Just t -> (gid t : targeted, (gid a, gid t) : pairs)
            Nothing -> (targeted, pairs)

attackPhase pairs m = foldl' doAtk m (sortOn (Down . ini . (m M.!)) (map fst pairs))
  where
    tM = M.fromList pairs
    doAtk curM aid = fromMaybe curM $ do
        a <- M.lookup aid curM
        guard (units a > 0)
        tid <- M.lookup aid tM
        t <- M.lookup tid curM
        let d = calcDamage a t
        return $ M.insert tid (t { units = max 0 (units t - (d `div` hp t)) }) curM

battle groups = loop (M.fromList [(gid g, g) | g <- groups])
  where
    loop m =
        let gs = M.elems m
            ts = selectTargets gs
            m' = attackPhase ts m
            before = sum (map units gs)
            after = sum (map units (M.elems m'))
            aliveMap = M.filter ((>0).units) m'
            armies = nub (map army (M.elems aliveMap))
        in if before == after then (-1, after)
           else if length armies <= 1 then (if null armies then (0,0) else (head armies, sum (map units (M.elems aliveMap))))
           else loop aliveMap

main = do
    input <- readFile "input.txt"
    let (immPart, infPart) = break (=="Infection:") (lines input)
        parseA aId base ls = [parseGroup aId (base + i) l | (i, l) <- zip [0..] ls, not (null l) && any isDigit l]
        gs = parseA 1 0 immPart ++ parseA 2 100 infPart
        findMin boost =
            let boosted = map (\g -> if army g == 1 then g { atk = atk g + boost } else g) gs
                (winner, score) = battle boosted
            in if winner == 1 then score else findMin (boost + 1)
    print (findMin 0)
