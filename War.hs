{-# OPTIONS -Wall #-} 
import System.Random
import Data.Char
import Text.Read

data Suit = DIAMONDS | CLUBS | HEARTS | SPADES 
  deriving (Eq, Ord, Enum, Bounded)
  
instance Show Suit where
  show DIAMONDS = "D"
  show CLUBS    = "C"
  show HEARTS   = "H"
  show SPADES   = "S"
  
data Value = TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN 
              | J | Q | K | A deriving (Eq, Ord, Enum, Bounded)

instance Show Value where
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"
  show x = show $ fromEnum x + 2
  
data Card = Card Suit Value deriving (Bounded)

instance Ord Card where
  compare (Card _ v1) (Card _ v2) = compare v1 v2

instance Eq Card where
  (Card _ v1) == (Card _ v2) = v1 == v2

instance Enum Card where
  fromEnum (Card s n) = 13*(fromEnum s) + fromEnum n
  toEnum n 
    | n > 51 = error "Your card value is too high (should be 0-51)"
    | otherwise = let (s,v) = n `divMod` 13 
                  in Card (toEnum s) (toEnum v)

instance Show Card where
  show (Card s v) = show v ++ show s
  
type Shoe = [Card]

fillShoe :: Int -> Shoe
fillShoe n 
  | n < 1 = error "We can't play without decks"
  | n < 9 = concat $ replicate n $ map toEnum [0..51]
  | otherwise = error "The shoe can't hold that many cards"

remove :: Int -> Shoe -> Shoe
remove n s = let (xs,ys) = splitAt (n-1) s
             in head ys : xs ++ tail ys

cut :: RandomGen a => a -> Shoe -> Shoe -- Could also use RandomGen for shuffle
cut gen s = let (xs,ys) = splitAt (fst $ randomR (0,length s) gen) s
            in ys ++ xs 
  
shuffle :: Int -> Shoe -> IO Shoe
shuffle _ [] = return []
shuffle n s = do
  let (x:xs) = remove n s 
  newGen <- newStdGen
  ys <- shuffle (fst $ randomR (1, length xs) newGen) xs
  return (x:ys)
            
main :: IO ()           
main = do
  putStrLn "How many decks should we play with?"
  numString <- getLine
  let numDecks = readMaybe numString
  case numDecks of 
    Nothing -> do putStrLn "Error: You must enter a number. Please play again later."
    Just n -> do 
      let startShoe = fillShoe n
          money = 200
      gen <- getStdGen
      shoe <- shuffle (fst $ randomR (1, length startShoe) gen) (startShoe)
      again <- playGame shoe money
      if again then main else return ()
  
playGame :: Shoe -> Int -> IO Bool
playGame s m = do
  (newShoe, remMoney) <- playHand s m
  putStrLn $ "Remaining Cards: " ++ show (length (newShoe))
  if remMoney > 0 && length newShoe > 1
    then do 
      putStrLn $ "You currently have: $" ++ show remMoney
      playGame newShoe remMoney 
    else do 
      putStrLn $ "You finished with: $" ++ show remMoney
      putStrLn "Would you like to play again? Enter \"y\" for yes, or anything else for no."
      response <- getLine
      return $ readResp response

readResp :: String -> Bool         
readResp x 
  | map toLower x == "y" = True
  | otherwise = False
  
playHand :: Shoe -> Int -> IO (Shoe,Int)
playHand shoe money = do
  putStrLn "How much would you like to bet?"
  bet <- getLine
  let numBet = readMaybe bet
  if not (validBet money numBet) 
    then do 
      putStrLn "Invalid bet, please try again"
      playHand shoe money
    else do 
      (cashChange,remS) <- evalHand numBet $ getCards shoe  
      return (remS, money + cashChange)
  
getCards :: Shoe -> (Card,Card,Shoe)
getCards shoe = (pc,dc,remS)
  where (pc:dc:remS) = shoe
  
evalHand :: Maybe Int -> (Card,Card,Shoe) -> IO (Int,Shoe)
evalHand (Just b) (p,d,r) 
  | p < d = do
      putStrLn $ "You Lose HAHAHA\nYou had: " ++ show p ++ "\nThe dealer had: " 
        ++ show d
      return (negate b, r)
  | p > d = do
      putStrLn $ "You are a winner!\nYou had: " ++ show p ++ "\nThe dealer had: " 
        ++ show d
      return (b, r)
  | otherwise = do
      putStrLn $ "You had: " ++ show p ++ "\nThe dealer had: " 
        ++ show d ++ "\n It's time for WAR!"
      if (length r > 1) 
        then evalHand (Just (2*b)) $ getCards r 
        else do 
          putStrLn "Oh no, we're out of cards. Game over."
          return (0,[])
evalHand Nothing _ = error "Invalid bet"

validBet :: Int -> Maybe Int -> Bool      
validBet _ Nothing = False
validBet m (Just b) 
  | b < 0 = False
  | b <= m = True
  | otherwise = False
