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

-- B1

-- Returns a new hand with the card added to the bottom of the provided hand
addToBottom :: Card -> Hand -> Hand
addToBottom card Empty = Add card Empty
addToBottom card (Add top rest) = Add top (addToBottom card rest)

-- Returns a new hand with the second hand appended to the first
(<+) :: Hand -> Hand -> Hand
hand <+ Empty = hand
hand <+ (Add card rest) = addToBottom card hand <+ rest

-- Defines the associative property of the (<+) operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool 
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- Property that the size of two hands (<+)ed together is equal to the sum of both hands
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2

-- B2

-- All possible numeric ranks of a valid card
numerics :: [Rank]
numerics = [Numeric i | i <- [2..10]]

-- All possible ranks of a valid card
ranks :: [Rank]
ranks = numerics ++ [Jack, Queen, King, Ace]

-- All possible suits of a valid card
suits :: [Suit]
suits = [Hearts, Spades, Diamonds, Clubs]

-- All possible cards in a standard deck
allCards :: [Card]
allCards = [Card rank suit | suit <- suits, rank <- ranks]

-- A full deck of cards, unshuffled, with exactly one of each possible card in a standard deck
fullDeck :: Hand
fullDeck = foldr addToBottom Empty allCards

-- B3

-- Draw a card from a deck and add it to the second hand.
-- Returns a pair where the left entry is the remaining deck with the card removed,
-- and the right entry is the new hand with the card added.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card rest) hand = (rest, Add card hand)

-- B4

-- Returns the bank's hand after the bank plays from the provided deck.
-- According to the rules in the specification, the bank draws from the deck
-- until its score is 16 or higher.
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- Same as `playBank`, but with the bank's (possibly non-empty) starting hand as the second argument
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value biggerHand >= 16 then biggerHand
    else playBankHelper smallerDeck biggerHand
    where (smallerDeck,biggerHand) = draw deck hand

-- B5

-- Returns a random permutation of the provided deck
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g hand = case maybeCard of
        Nothing   -> hand
        Just card -> addToBottom card (shuffleDeck g' rest)
    where (n, g')           = randomR (1, size hand) g
          (maybeCard, rest) = removeNthCard hand n

-- Removes the card from the provided deck at the provided position.
-- Returns a pair whose left value is the removed card (possibly `Nothing`, if no card exists at that position),
-- and whose right value is the rest of the deck with that card removed (or the original deck if no card was removed).
removeNthCard :: Hand -> Int -> (Maybe Card, Hand)
removeNthCard deck n = (mCard, first <+ rest)
    where (first, mCard, rest) = splitDeckAt deck n

-- Obtains the card from the provided deck at the provided position and splits the deck into two decks,
-- one containing the cards appearing before the "split" card, and one containing those that appear after.
-- Returns a triple where:
--     the first entry is all cards appearing before the split card
--     the second entry is the split card appearing at the provided position (or `Nothing` if none exists)
--     the third entry is all cards appearing after the split card
-- If the provided position is less than 1, the first entry will be empty and the third will be the full deck.
-- If the provided position is greater than the size of the deck, the first entry will be the full deck and the
-- third entry will be empty.
splitDeckAt :: Hand -> Int -> (Hand, Maybe Card, Hand)
splitDeckAt Empty n = (Empty, Nothing, Empty)
splitDeckAt hand n | n < 1 = (Empty, Nothing, hand)
splitDeckAt (Add card rest) n
    | n == 1    = (Empty, Just card, rest)
    | otherwise = (Add card top, nthCard, bottom)
        where (top, nthCard, bottom) = splitDeckAt rest (n - 1)

-- Whether the provided card is in the provided hand.
belongsTo :: Card -> Hand -> Bool 
c `belongsTo` Empty = False 
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property that a card must belong to a deck iff the card belongs to that deck after it is shuffled
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool 
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Property that a shuffled deck must be the same size as the original deck
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size (shuffleDeck g hand) == (size hand)

--B6

implementation = Interface
    { iFullDeck = fullDeck
    , iValue    = value
    , iDisplay  = display
    , iGameOver = gameOver
    , iWinner   = winner 
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffleDeck
    }

-- Starts an interactive shell to play the blackjack game
main :: IO () 
main = runGame implementation
