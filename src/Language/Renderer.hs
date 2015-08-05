module Language.Renderer where

import Language.Parser
import Debug.Trace

data SMSState = State String Time Int Int

render :: [ChunkList] -> String
render xs = renderInner xs (getDefaults xs)

renderInner :: [ChunkList] -> SMSState -> String
renderInner (SMSList x:xs) st =
    let (st', str) = renderSMS x st in
    str ++ renderInner xs st'
renderInner (PRawList x:xs) st = renderText x ++ renderInner xs st
renderInner _ _ = []

renderSMS :: [SMSLine] -> SMSState -> (SMSState, String)
renderSMS xs st = renderSMSInner st "" xs

sub :: Int -> Int
sub x
    | x <= 0 = 0
    | otherwise = x - 1

-- | Name String
-- | TextLine String
-- | AbsTime Time
-- | RelTime Int
renderSMSInner :: SMSState -> String -> [SMSLine] -> (SMSState, String)
renderSMSInner st acc [] = (st, acc)
renderSMSInner st@(State name time c b) acc (line:xs) =
    let (st', acc') =
         case line of
             Name s ->
                 let ns = (State s time 2 b) in
                 (ns, acc)
             TextLine s ->
                 let ns = (State name time (sub c) (sub b)) in
                 let acs = acc ++ (put st s) in
                 (ns, acs)
             AbsTime t ->
                 let ns = (State name t c 2) in
                 (ns, acc)
             RelTime t ->
                 let ns = (State name (addTime time t) c 2) in
                 (ns, acc)
             Blank -> (st, acc)
    in
    renderSMSInner st' acc' xs

put :: SMSState -> String -> String
put (State name time c b) msg =
    let myname = if c > 1 then name else "" in
    let (tbox,dbox) = if name == "me"
                      then ("rightbox", "rightdate")
                      else ("leftbox", "leftdate")
    in
    let tt =
         if b > 1
         then concat $ ["\\"
                       , dbox
                       , "{"
                       , rTime time
                       , "}\n"
                       ]
         else ""
    in
    concat [ "\\noindent\n"
           , "\\"
           , tbox
           ,"{"
           , myname
           , "}{"
           , msg
           , "}\n"
           ] ++ tt

rTime :: Time -> String
rTime (Time h m p) = show h ++ ":" ++ show m ++ " " ++ rTimePeriod p
  where
    rTimePeriod AM = "am"
    rTimePeriod PM = "pm"


renderText :: [String] -> String
renderText = unlines

getDefaults :: [ChunkList] -> SMSState
getDefaults (PRawList _:xs) = getDefaults xs
getDefaults (SMSList ls:xs) = accumDefaults Nothing Nothing ls
  where
    accumDefaults :: Maybe String -> Maybe Time -> [SMSLine] -> SMSState
    accumDefaults (Just s) (Just t) xs = State s t 2 2
    accumDefaults _ b (Name s:xs) = accumDefaults (Just s) b xs
    accumDefaults a _ (AbsTime t:xs) = accumDefaults a (Just t) xs
    accumDefaults a b (_:xs) = accumDefaults a b xs
    accumDefaults a b xs = error "accumDefaults"


addTime :: Time -> Int -> Time
addTime t i =
    let (h, m) = to24 t in
    let nm = m + i in
    let h' = if nm >= 60 then h + 1 else h in
    let m' = if nm >= 60 then nm - 60 else nm in
    let h'' = if h' > 23 then 0 else h' in
    from24 h'' m'

to24 :: Time -> (Int,Int)
to24 (Time h m AM) = (if h == 12 then 0 else h, m)
to24 (Time h m PM) = (if h == 12 then 12 else h + 12, m)

from24 :: Int -> Int -> Time
from24 h m =
    let (h', p) =
         if h == 0
         then (12,AM)
         else if h == 12
         then (12,PM)
         else if h > 12
         then (h-12, PM)
         else (h, AM)
    in
    Time h' m p



