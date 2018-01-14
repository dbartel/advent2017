import Data.List (elemIndex)

type Block = Int
type Bank = [Block]

indexOfMax :: Bank -> (Int, Block)
indexOfMax b = let maxim = (foldl1 max b)
                   maybeIndex = (elemIndex maxim b)
               in case (maybeIndex) of
                    (Just i) -> (i, maxim)
                    Nothing -> error "Bad index"

nextIndex :: Bank -> Int -> Int
nextIndex b i = if ((i + 1) >= (length b)) then 0 else i + 1

redistributeBank :: Bank -> Int -> Int -> Bank
redistributeBank bank 0 startIndex = bank
redistributeBank bank amount startIndex = let
  (before, _:after) = splitAt startIndex bank
  newValue = (bank !! startIndex) + 1
  newIndex = nextIndex bank startIndex
  newAmount = amount - 1
  in redistributeBank (before ++ [newValue] ++ after) newAmount newIndex

bankOperation :: Bank -> Bank
bankOperation bank = let
  (maxIndex, val) = indexOfMax bank
  (before, _: after) = splitAt maxIndex bank
  startBank = before ++ [0] ++ after
  startIndex = nextIndex startBank maxIndex
  in redistributeBank startBank val startIndex

countRedistributions :: Bank -> [Bank] -> Int -> (Bank, Int)
countRedistributions bank previous count = case (elemIndex bank previous) of
  Just _ -> (bank, count)
  Nothing -> countRedistributions (bankOperation bank) (previous ++ [bank]) (count + 1)

countStates :: Bank -> [Bank] -> Int
countStates bank previous = case (elemIndex bank previous) of
  Just a -> (length previous) - a
  Nothing -> countStates (bankOperation bank) (previous ++ [bank])

daySixA :: Bank -> (Bank, Int)
daySixA b = countRedistributions b [] 0

daySixB :: Bank -> Int
daySixB b = countStates b []

  

  
