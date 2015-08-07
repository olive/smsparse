module Language.Parser where


import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Text.Parsec hiding (Line)
import Text.Parsec.ByteString
import Debug.Trace

data Time = Time Int Int TimePeriod
  deriving (Show)

data TimePeriod = AM | PM
  deriving (Show)


data SMSLine = Name String
             | TextLine String
             | AbsTime Time
             | RelTime Int
             | Blank
  deriving (Show)

data Chunk = Raw String
           | Divider
  deriving (Show)

data PChunk = PRaw String
            | SMS SMSLine
  deriving (Show)

data PState = SMSState | RawState
  deriving (Show)

data ChunkList = PRawList [String]
               | SMSList [SMSLine]
  deriving (Show)

isRaw :: PChunk -> Bool
isRaw (PRaw _) = True
isRaw _ = False

extractRaw :: PChunk -> String
extractRaw (PRaw s) = s
extractRaw _ = error "extractRaw"


extractSMS :: PChunk -> SMSLine
extractSMS (SMS s) = s
extractSMS _ = error "extractSMS"

parse :: [String] -> [ChunkList]
parse xs =
    let chunks = pChunk <$> xs in
    let x = parsePChunks RawState chunks [] in
    x


pChunk :: String -> Chunk
pChunk "%%%" = Divider
pChunk s = Raw s

parsePChunks :: PState -> [Chunk] -> [PChunk] -> [ChunkList]
parsePChunks SMSState (Divider:xs) cur =
    SMSList (extractSMS <$> cur) : parsePChunks RawState xs []
parsePChunks RawState (Divider:xs) cur =
    PRawList (extractRaw <$> cur) : parsePChunks SMSState xs []
parsePChunks SMSState (Raw s:xs) cur =
    let next = (SMS . parseSMSLine) s in
    parsePChunks SMSState xs (cur ++ [next])
parsePChunks RawState (Raw s:xs) cur =
    let next = PRaw s in
    parsePChunks RawState xs (cur ++ [next])
parsePChunks RawState _ cur = [PRawList (extractRaw <$> cur)]
parsePChunks SMSState _ cur = [SMSList (extractSMS <$> cur)]

parseSMSLine :: String -> SMSLine
parseSMSLine s
    | isName s = Name . tail $ s
    | hasColon s = pAbsTime s
    | isSpaced s = TextLine . tail . tail $ s
    | isBlank s = Blank
    | otherwise = pRel s

isBlank :: String -> Bool
isBlank [] = True
isBlank _ = False

isSpaced :: String -> Bool
isSpaced (' ':' ':_) = True
isSpaced _ = False

hasColon :: String -> Bool
hasColon (' ':xs) = False
hasColon (':':xs) = True
hasColon (_:xs) = hasColon xs
hasColon _ = False

isName :: String -> Bool
isName (':':xs) = True
isName _ = False

pAbsTime :: String -> SMSLine
pAbsTime s = pNum1 s
  where
    pNum1 :: String -> SMSLine
    pNum1 (a:b:':':xs) = pNum2 [a,b] xs
    pNum1 (a:':':xs) = pNum2 [a] xs
    pNum1 _ = error "pNum1"

    pNum2 :: String -> String -> SMSLine
    pNum2 s xs =
        let num = parseInt s in
        let (num2, p) = pNum3 xs in
        AbsTime $ Time num num2 p

    pNum3 :: String -> (Int, TimePeriod)
    pNum3 (a:b:[]) = (parseInt [a,b], AM)
    pNum3 (a:b:x:['m']) = (parseInt [a,b], pPeriod x)
    pNum3 (a:b:x:['M']) = (parseInt [a,b], pPeriod x)
    pNum3 s = error $ "pNum3:" ++ s

    pPeriod :: Char -> TimePeriod
    pPeriod 'a' = AM
    pPeriod 'A' = AM
    pPeriod 'p' = PM
    pPeriod 'P' = PM
    pPeriod _ = error "pPeriod"

pRel :: String -> SMSLine
pRel = RelTime . parseInt


parseInt :: String -> Int
parseInt s =
    let parses ::[(Int,String)]
        parses = reads s
    in
    case parses of
        [] -> error ("Failed to parse int of: " ++ s)
        (i,_):_ -> i
