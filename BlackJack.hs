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

--B1

addToBottom :: Card -> Hand -> Hand
addToBottom card Empty = Add card Empty
addToBottom card (Add top rest) = Add top (addToBottom card rest)

(<+) :: Hand -> Hand -> Hand
hand <+ Empty = hand
hand <+ (Add card rest) = addToBottom card hand <+ rest

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

--B2
numerics :: [Rank]
numerics = [Numeric i | i <- [2..10]]

ranks :: [Rank]
ranks = numerics ++ [Jack, Queen, King, Ace]

suits :: [Suit]
suits = [Hearts, Spades, Diamonds, Clubs]

allCards :: [Card]
allCards = [Card rank suit | suit <- suits, rank <- ranks]

fullDeck :: Hand
fullDeck = foldr addToBottom Empty allCards

--B3
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card rest) hand = (rest, Add card hand)

--B4
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value biggerHand >= 16 then biggerHand
    else playBankHelper smallerDeck biggerHand
    where (smallerDeck,biggerHand) = draw deck hand

shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g hand = case maybeCard of
        Nothing   -> hand
        Just card -> addToBottom card (shuffleDeck g' rest)
    where (n, g')           = randomR (1, size hand) g
          (maybeCard, rest) = removeNthCard hand n

removeNthCard :: Hand -> Int -> (Maybe Card, Hand)
removeNthCard deck n = (mCard, first <+ rest)
    where (first, mCard, rest) = splitDeckAt deck n

splitDeckAt :: Hand -> Int -> (Hand, Maybe Card, Hand)
splitDeckAt Empty n = (Empty, Nothing, Empty)
splitDeckAt hand n | n < 1 = (Empty, Nothing, hand)
splitDeckAt (Add card rest) n
    | n == 1    = (Empty, Just card, rest)
    | otherwise = (Add card top, nthCard, bottom)
        where (top, nthCard, bottom) = splitDeckAt rest (n - 1)

belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size (shuffleDeck g hand) == (size hand)
