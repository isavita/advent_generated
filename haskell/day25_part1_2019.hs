-- day25_part1_2019.hs
-- AoC 2019 Day 25 (Intcode text adventure) - automated solver
--
-- Build:
--   ghc -O2 -o day25 day25_part1_2019.hs
-- Run:
--   ./day25 input.txt
--
-- Or:
--   runghc day25_part1_2019.hs input.txt

{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), (|>))
import Data.Char (isSpace, isDigit, chr, ord)
import Data.Bits ((.&.), shiftL)
import Data.List (isPrefixOf, isSuffixOf, find, sort)
import System.Environment (getArgs)

--------------------------------------------------------------------------------
-- Intcode Emulator (sparse memory, ASCII I/O)
--------------------------------------------------------------------------------

data Emu = Emu
  { mem :: !(IM.IntMap Integer)
  , inp :: !(Seq Integer)
  , ip  :: !Int
  , rb  :: !Integer
  } deriving (Show)

data Status
  = Halted
  | Output !Int
  | WaitingForInput
  deriving (Show)

emuNew :: [Integer] -> Emu
emuNew program =
  let m = IM.fromList (zip [0..] program)
  in Emu { mem = m, inp = Seq.empty, ip = 0, rb = 0 }

memGet :: Emu -> Int -> Integer
memGet e addr = IM.findWithDefault 0 addr (mem e)

memSet :: Emu -> Int -> Integer -> Emu
memSet e addr v = e { mem = IM.insert addr v (mem e) }

writeString :: Emu -> String -> Emu
writeString e s =
  e { inp = foldl (\q ch -> q |> toInteger (ord ch)) (inp e) s }

pow10 :: Int -> Integer
pow10 n = go 1 n
  where
    go !acc 0 = acc
    go !acc k = go (acc * 10) (k - 1)

modeOf :: Integer -> Int -> Integer
modeOf instr offset = (instr `div` pow10 (offset + 1)) `mod` 10

getParam :: Emu -> Integer -> Int -> Integer
getParam e instr offset =
  let m = modeOf instr offset
      p = memGet e (ip e + offset)
  in case m of
       0 -> memGet e (fromIntegral p)
       1 -> p
       2 -> memGet e (fromIntegral (rb e + p))
       _ -> error ("Unknown parameter mode: " ++ show m)

getWriteAddr :: Emu -> Integer -> Int -> Int
getWriteAddr e instr offset =
  let m = modeOf instr offset
      p = memGet e (ip e + offset)
      addr =
        case m of
          0 -> p
          2 -> rb e + p
          _ -> error ("Invalid mode for writing: " ++ show m)
  in if addr < 0 then error "Negative write address" else fromIntegral addr

-- Run until one event: Output / WaitingForInput / Halted
emulate :: Emu -> (Emu, Status)
emulate e0 = go e0
  where
    go !e =
      let instr  = memGet e (ip e)
          opcode = instr `mod` 100
      in case opcode of
           1 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 c = getWriteAddr e instr 3
                 e' = memSet e c (a + b)
             in go e' { ip = ip e + 4 }
           2 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 c = getWriteAddr e instr 3
                 e' = memSet e c (a * b)
             in go e' { ip = ip e + 4 }
           3 ->
             case inp e of
               Seq.Empty -> (e, WaitingForInput)
               (v :<| rest) ->
                 let c  = getWriteAddr e instr 1
                     e' = memSet e c v
                 in go e' { inp = rest, ip = ip e + 2 }
           4 ->
             let a = getParam e instr 1
             in (e { ip = ip e + 2 }, Output (fromIntegral a))
           5 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 newIp = if a /= 0 then fromIntegral b else ip e + 3
             in go e { ip = newIp }
           6 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 newIp = if a == 0 then fromIntegral b else ip e + 3
             in go e { ip = newIp }
           7 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 c = getWriteAddr e instr 3
                 e' = memSet e c (if a < b then 1 else 0)
             in go e' { ip = ip e + 4 }
           8 ->
             let a = getParam e instr 1
                 b = getParam e instr 2
                 c = getWriteAddr e instr 3
                 e' = memSet e c (if a == b then 1 else 0)
             in go e' { ip = ip e + 4 }
           9 ->
             let a = getParam e instr 1
             in go e { rb = rb e + a, ip = ip e + 2 }
           99 -> (e, Halted)
           _  -> error ("Unknown opcode: " ++ show opcode ++ " at ip=" ++ show (ip e))

--------------------------------------------------------------------------------
-- World / Solver State
--------------------------------------------------------------------------------

data Mode = Explore | Navigate | Test deriving (Eq, Show)

data Room = Room
  { rName  :: !String
  , rConns :: !(M.Map String (Maybe String))  -- dir -> destination room name (Nothing = unexplored)
  } deriving (Show)

data St = St
  { stEmu         :: !Emu
  , stWorld       :: !(M.Map String Room)
  , stInventory   :: !(M.Map String Bool)
  , stMode        :: !Mode
  , stCurrentRoom :: !(Maybe String)
  , stCheckpoint  :: !(Maybe String)
  , stFloor       :: !(Maybe String)
  , stTestDir     :: !String
  , stPath        :: ![String]         -- stack (head = previous room)
  , stAvailable   :: ![String]
  , stItemMask    :: !Int
  , stLastRoom    :: !(Maybe String)
  , stLastItems   :: ![String]
  , stLastDir     :: !String
  , stOutRev      :: ![Char]           -- output buffer, reversed
  } deriving (Show)

opposite :: String -> String
opposite d = case d of
  "north" -> "south"
  "south" -> "north"
  "west"  -> "east"
  "east"  -> "west"
  _       -> error ("Unknown direction: " ++ d)

blacklist :: String -> Bool
blacklist item =
  item `elem`
    [ "photons"
    , "escape pod"
    , "molten lava"
    , "infinite loop"
    , "giant electromagnet"
    ]

emptyRoom :: String -> Room
emptyRoom name = Room name M.empty

getRoom :: String -> M.Map String Room -> Room
getRoom name world = M.findWithDefault (emptyRoom name) name world

putRoom :: Room -> M.Map String Room -> M.Map String Room
putRoom r world = M.insert (rName r) r world

ensureRoom :: String -> St -> St
ensureRoom name st =
  if M.member name (stWorld st) then st
  else st { stWorld = M.insert name (emptyRoom name) (stWorld st) }

setDoor :: String -> St -> St
setDoor dir st =
  case stCurrentRoom st of
    Nothing -> st
    Just rn ->
      let room  = getRoom rn (stWorld st)
          conns = rConns room
          conns' = M.insertWith (\_ old -> old) dir Nothing conns
          room' = room { rConns = conns' }
      in st { stWorld = putRoom room' (stWorld st) }

setConn :: String -> String -> String -> St -> St
setConn from dir to st =
  let room  = getRoom from (stWorld st)
      conns = rConns room
      room' = room { rConns = M.insert dir (Just to) conns }
  in st { stWorld = putRoom room' (stWorld st) }

updateConnections :: St -> St
updateConnections st =
  case (stLastRoom st, stCurrentRoom st) of
    (Just lastN, Just curN) | stLastDir st /= "" ->
      let lastR = getRoom lastN (stWorld st)
          curR  = getRoom curN  (stWorld st)
          existing = M.lookup (stLastDir st) (rConns lastR)
          shouldLink =
            case existing of
              Nothing      -> True
              Just Nothing -> True
              Just (Just _) -> False
      in if not shouldLink then st
         else
           let lastR' = lastR { rConns = M.insert (stLastDir st) (Just curN) (rConns lastR) }
               curR'  = curR  { rConns = M.insert (opposite (stLastDir st)) (Just lastN) (rConns curR) }
               w' = putRoom curR' (putRoom lastR' (stWorld st))
           in st { stWorld = w' }
    _ -> st

handleAlert :: St -> St
handleAlert st =
  let st' =
        if stMode st == Explore
          then
            let path' = case stPath st of
                          []     -> []
                          (_:xs) -> xs
                st1 = st { stPath = path'
                         , stCheckpoint = stLastRoom st
                         , stFloor = stCurrentRoom st
                         , stTestDir = stLastDir st
                         }
            in case (stCheckpoint st1, stFloor st1) of
                 (Just cp, Just fl) | stTestDir st1 /= "" ->
                   setConn cp (stTestDir st1) fl st1
                 _ -> st1
          else st
  in st' { stLastRoom = Nothing, stLastItems = [], stLastDir = "" }

--------------------------------------------------------------------------------
-- Parsing output
--------------------------------------------------------------------------------

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

isRoomHeader :: String -> Bool
isRoomHeader line = "== " `isPrefixOf` line && " ==" `isSuffixOf` line && length line >= 6

roomNameFromHeader :: String -> String
roomNameFromHeader line = take (length line - 6) (drop 3 line)

parseListBlock :: [String] -> ([String], [String])
parseListBlock ls =
  let go acc xs =
        case xs of
          [] -> (reverse acc, [])
          (l:rest) ->
            let t = trim l
            in if t == ""
                 then (reverse acc, xs)  -- stop before blank; caller will skip blanks
                 else if "- " `isPrefixOf` t
                        then go (drop 2 t : acc) rest
                        else go acc rest
  in go [] ls

skipBlanks :: [String] -> [String]
skipBlanks = dropWhile (\l -> trim l == "")

skipToBlank :: [String] -> [String]
skipToBlank xs = dropWhile (\l -> trim l /= "") xs

processOutput :: St -> String -> (St, [String])
processOutput st0 out =
  let ls = lines out
      go st items xs =
        case xs of
          [] -> (st, items)
          (l:rest0) ->
            let line = trim l
            in if line == "" || line == "Command?"
                 then go st items rest0
               else if isRoomHeader line
                 then
                   let name = roomNameFromHeader line
                       st1  = ensureRoom name st
                       st2  = st1 { stCurrentRoom = Just name }
                       rest1 = skipToBlank rest0
                   in go st2 [] (skipBlanks rest1)
               else if line == "Doors here lead:"
                 then
                   let (dirs, rest1) = parseListBlock rest0
                       st1 = foldl (flip setDoor) st dirs
                       rest2 = skipToBlank rest1
                   in go st1 items (skipBlanks rest2)
               else if line == "Items here:"
                 then
                   let (its, rest1) = parseListBlock rest0
                       rest2 = skipToBlank rest1
                   in go st its (skipBlanks rest2)
               else if "You take the " `isPrefixOf` line && "." `isSuffixOf` line
                 then
                   let pref = "You take the "
                       taken = take (length line - length pref - 1) (drop (length pref) line)
                       st1 = st { stInventory = M.insert taken True (stInventory st) }
                   in case stLastRoom st1 of
                        Nothing -> go st1 items rest0
                        Just lr ->
                          let items' = filter (/= taken) (stLastItems st1)
                              st2 = st1 { stCurrentRoom = Just lr }
                          in go st2 items' rest0
               else if "You drop the " `isPrefixOf` line && "." `isSuffixOf` line
                 then
                   let pref = "You drop the "
                       dropped = take (length line - length pref - 1) (drop (length pref) line)
                       st1 = st { stInventory = M.insert dropped False (stInventory st) }
                   in case stLastRoom st1 of
                        Nothing -> go st1 items rest0
                        Just lr ->
                          let items' = stLastItems st1 ++ [dropped]
                              st2 = st1 { stCurrentRoom = Just lr }
                          in go st2 items' rest0
               else if "A loud, robotic voice says \"Alert!" `isPrefixOf` line
                 then go (handleAlert st) items rest0
               else go st items rest0
  in go st0 [] ls

extractCode :: String -> Maybe String
extractCode out =
  let pref = "\"Oh, hello! You should be able to get in by typing "
      go [] = Nothing
      go (l:ls) =
        let line = trim l
        in if pref `isPrefixOf` line
             then
               let rest = drop (length pref) line
                   ds = takeWhile isDigit rest
               in if null ds then go ls else Just ds
             else go ls
  in go (lines out)

--------------------------------------------------------------------------------
-- Pathfinding
--------------------------------------------------------------------------------

findPath :: St -> String -> String -> Maybe [String]
findPath st from to =
  let q0 = Seq.singleton (from, [from])
      visited0 = M.singleton from True
      bfs q visited =
        case Seq.viewl q of
          Seq.EmptyL -> Nothing
          (cur, path) Seq.:< qrest ->
            if cur == to then Just path
            else
              let room = getRoom cur (stWorld st)
                  nbrs = [ n | Just n <- M.elems (rConns room), M.notMember n visited ]
                  visited' = foldl (\m n -> M.insert n True m) visited nbrs
                  q' = foldl (\qq n -> qq |> (n, path ++ [n])) qrest nbrs
              in bfs q' visited'
  in bfs q0 visited0

dirTo :: St -> String -> String -> Maybe String
dirTo st from to =
  let room = getRoom from (stWorld st)
      matches = [ dir | (dir, Just n) <- M.toList (rConns room), n == to ]
  in case matches of
       (d:_) -> Just d
       []    -> Nothing

chooseUnexplored :: St -> String -> Maybe String
chooseUnexplored st rn =
  let room = getRoom rn (stWorld st)
      con  = rConns room
      pref = ["north","south","west","east"]
      pick [] = Nothing
      pick (d:ds) =
        case M.lookup d con of
          Just Nothing -> Just d
          _            -> pick ds
  in case pick pref of
       Just d  -> Just d
       Nothing ->
         case [ d | (d, Nothing) <- M.toList con ] of
           (d:_) -> Just d
           []    -> Nothing

--------------------------------------------------------------------------------
-- Action selection
--------------------------------------------------------------------------------

sendCmd :: St -> String -> St
sendCmd st cmd = st { stEmu = writeString (stEmu st) cmd }

-- IMPORTANT: bootstrap if current room is unknown (some runs can start with a prompt-only output)
bootstrapMove :: St -> St
bootstrapMove st =
  let dir = "north"
      st1 = st { stLastDir = dir }
  in sendCmd st1 (dir ++ "\n")

exploreStep :: St -> [String] -> St
exploreStep st items =
  case find (not . blacklist) items of
    Just it ->
      sendCmd st ("take " ++ it ++ "\n")
    Nothing ->
      case stCurrentRoom st of
        Nothing -> bootstrapMove st
        Just cur ->
          case chooseUnexplored st cur of
            Just dir ->
              let st1 = st { stPath = cur : stPath st, stLastDir = dir }
              in sendCmd st1 (dir ++ "\n")
            Nothing ->
              case stPath st of
                (prev:rest) ->
                  case dirTo st cur prev of
                    Just back ->
                      let st1 = st { stPath = rest, stLastDir = back }
                      in sendCmd st1 (back ++ "\n")
                    Nothing ->
                      error ("Cannot go from \"" ++ cur ++ "\" to \"" ++ prev ++ "\"")
                [] ->
                  case (stCheckpoint st, stFloor st) of
                    (Just cp, Just _) ->
                      case findPath st cur cp of
                        Nothing -> error "No path to checkpoint"
                        Just p  ->
                          let nav = case p of (_:xs) -> xs; [] -> []
                          in st { stMode = Navigate, stPath = nav }
                    _ -> bootstrapMove st

navigateStep :: St -> St
navigateStep st =
  case stCurrentRoom st of
    Nothing -> bootstrapMove st
    Just cur ->
      case stPath st of
        [] ->
          let avail = sort [ it | (it, True) <- M.toList (stInventory st) ]
          in st { stMode = Test, stAvailable = avail, stItemMask = 0 }
        (next:rest) ->
          case dirTo st cur next of
            Just dir ->
              let st1 = st { stPath = rest, stLastDir = dir }
              in sendCmd st1 (dir ++ "\n")
            Nothing ->
              error ("Cannot go from \"" ++ cur ++ "\" to \"" ++ next ++ "\"")

testStep :: St -> St
testStep st =
  let items = stAvailable st
      mask  = stItemMask st
      maxMask = 1 `shiftL` length items
  in if mask >= maxMask
       then error ("No valid item combination found after " ++ show maxMask ++ " attempts")
       else
         case firstMismatch items 0 of
           Just (it, target) ->
             let action = if target then "take " else "drop "
             in sendCmd st (action ++ it ++ "\n")
           Nothing ->
             if stTestDir st == ""
               then bootstrapMove st
               else
                 let st1 = st { stItemMask = mask + 1 }
                 in sendCmd st1 (stTestDir st ++ "\n")
  where
    firstMismatch [] _ = Nothing
    firstMismatch (it:rest) idx =
      let target   = (stItemMask st .&. (1 `shiftL` idx)) /= 0
          curState = M.findWithDefault False it (stInventory st)
      in if curState /= target then Just (it, target) else firstMismatch rest (idx + 1)

--------------------------------------------------------------------------------
-- Main loop
--------------------------------------------------------------------------------

maxActions :: Int
maxActions = 30000

loop :: Int -> St -> IO ()
loop !actions st
  | actions > maxActions = error "Loop detected (action cap reached)"
  | otherwise =
      let (e', status) = emulate (stEmu st)
          st1 = st { stEmu = e' }
      in case status of
           Halted -> do
             let out = reverse (stOutRev st1)
             case extractCode out of
               Just code -> putStrLn code
               Nothing   -> error "Program halted but no code found in output."
           Output ch -> do
             let st2 = st1 { stOutRev = chr ch : stOutRev st1 }
             loop actions st2
           WaitingForInput -> do
             let outStr = reverse (stOutRev st1)
                 st2    = st1 { stOutRev = [] }
                 (st3, items) = processOutput st2 outStr
                 st4 = updateConnections st3
                 st5 =
                   st4 { stLastRoom  = stCurrentRoom st4
                       , stLastItems = items
                       , stLastDir   = ""   -- will be set by action choice
                       }
                 st6 =
                   case stMode st5 of
                     Explore  -> exploreStep st5 items
                     Navigate -> navigateStep st5
                     Test     -> testStep st5
             loop (actions + 1) st6

--------------------------------------------------------------------------------
-- Input parsing / entry point
--------------------------------------------------------------------------------

splitOnChar :: Char -> String -> [String]
splitOnChar _ [] = [""]
splitOnChar c (x:xs)
  | x == c    = "" : splitOnChar c xs
  | otherwise =
      let (y:ys) = splitOnChar c xs
      in (x:y) : ys

readProgram :: String -> [Integer]
readProgram txt =
  let t = trim txt
  in if null t then []
     else map (read . trim) (splitOnChar ',' t)

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
               (f:_) -> f
               []    -> "input.txt"
  txt <- readFile file
  let program = readProgram txt
      e0 = emuNew program
      st0 =
        St { stEmu = e0
           , stWorld = M.empty
           , stInventory = M.empty
           , stMode = Explore
           , stCurrentRoom = Nothing
           , stCheckpoint = Nothing
           , stFloor = Nothing
           , stTestDir = ""
           , stPath = []
           , stAvailable = []
           , stItemMask = 0
           , stLastRoom = Nothing
           , stLastItems = []
           , stLastDir = ""
           , stOutRev = []
           }
  loop 0 st0

