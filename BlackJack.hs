module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- A0
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
handAce = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty)
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]

-- A1
-- A string representing the card in a readable format
displayCard :: Card -> String
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

-- Strings representing each card in a hand in a readable format
display :: Hand -> String
display Empty = ""
display (Add card rest) = displayCard card ++ "\n" ++ display rest

-- A2
-- The maximum point value of a hand without going over 21, if possible
value :: Hand -> Integer
value hand = if score > 21 && numAces > 0 then
                score - 10*numAces else score
            where
                score = initialValue hand
                numAces = numberOfAces hand

-- The total point value of a hand, counting all aces as 11
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card rank _) rest) = valueRank rank + initialValue rest

-- Number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) rest) = 1 + numberOfAces rest
numberOfAces (Add _ rest) = numberOfAces rest

-- The point value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric value) = value
valueRank Ace = 11
valueRank _ = 10

-- A3
-- Whether the value of the hand is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4
-- Returns the winner, bank or guest, of the hand.
-- In order to win, the player must have a score higher than the bank but not higher than 21
winner :: Hand -> Hand -> Player
winner guestHand bankHand 
        | not (gameOver guestHand) && (guestValue > bankValue || gameOver bankHand) = Guest
        | otherwise = Bank  
        where guestValue = value guestHand 
              bankValue  = value bankHand
