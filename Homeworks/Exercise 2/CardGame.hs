module CardGame where

import Data.Char 

data Color = Red | Black deriving (Eq,Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq,Show)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Eq,Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq,Show)
data Move = Draw | Discard Card deriving (Eq,Show)

cardColor::Card -> Color
cardColor card = case suit card of
    Diamonds  -> Red
    Hearts    -> Red   
    _         -> Black

cardValue::Card -> Int
cardValue card = case rank card of 
    Ace        -> 11
    King       -> 10
    Jack       -> 10
    Queen      -> 10
    Num number -> number


--Removes given card from deck
removeCard::[Card] -> Card -> [Card]
removeCard [] card       = error "This card is not in the deck, deck is empty!"
removeCard [c] card      = if (c /= card) then error "This card is not in the deck." else []
removeCard (c:cs) card   = if (c == card) then cs else  (c : removeCard cs card)

--Checks if all cards are same colour
allSameColor::[Card] -> Bool
allSameColor []          = True
allSameColor [c1]        = True
allSameColor (c1:c2:cs)  = (cardColor c1 == cardColor c2)  && allSameColor cs

--Returns sum of the cards' values
sumCards::[Card] -> Int
sumCards []  = 0
sumCards cs  = sumValues cs 0
    where
        sumValues::[Card] -> Int -> Int
        sumValues [] acc     = acc
        sumValues (c:cs) acc = sumValues cs (acc + cardValue c)

--Calculates score
score::[Card] -> Int -> Int
score cards goal
    |(sumCards cards) > goal = checkColor cards (3 * ((sumCards cards) - goal))
    |otherwise               = checkColor cards (goal - (sumCards cards)) 
    where
        checkColor::[Card]->Int->Int
        checkColor cs number = if allSameColor cards then div number 2 else number

--Returns if there is any card left as a state
state::[Card]->Bool
state cards
    |cards == []        = False
    |otherwise          = True      

--Runs game with given cards,moves and goal.
runGame::[Card] -> [Move] -> Int -> Int
runGame cards moves goal = run cards [] moves goal (state cards)
    where
        run::[Card] -> [Card] -> [Move] -> Int -> Bool -> Int
        run _ heldCards [] goal _ = score heldCards goal
        run cards@(c:cs) heldCards moves@(m:ms) goal stateOfCards = case (m,stateOfCards) of
            (_, False)            -> score heldCards goal
            ((Discard c), True)   -> run cards (removeCard heldCards c) ms goal True
            (Draw, True)          -> if sumCards (c : heldCards) > goal then score (c : heldCards) goal 
                                        else run (removeCard cards c) (c : heldCards) ms goal (state cs)            
convertSuit::Char->Suit
convertSuit c
    |c == 'd' || c == 'D'  = Diamonds
    |c == 'h' || c == 'H'  = Hearts
    |c == 's' || c == 'S'  = Spades
    |c == 'c' || c == 'C'  = Clubs
    |otherwise             = error "Invalid character for suit."

convertRank::Char->Rank
convertRank c 
    |c =='1'                 = Ace
    |c =='j' || c =='J'      = Jack
    |c =='q' || c =='Q'      = Queen
    |c =='k' || c =='K'      = King
    |c =='t' || c =='T'      = Num 10
    |isDigit c               = Num (digitToInt c)
    |otherwise               = error "Invalid character for rank."

convertCard::Char->Char->Card
convertCard suit rank = Card (convertSuit suit) (convertRank rank)

--Reads input
readCards::[Card]->IO [Card]
readCards cards = do
    line <- getLine
    if line!!0 == '.' then return cards
        else if length line == 2 then readCards (convertCard (line!!0) (line!!1) : cards)
            else error "Card input is not in correct format."

--Decides moves from given characters
convertMove::Char->Char->Char->Move
convertMove m s r = case (toLower m,s,r) of
    ('d',_,_)   -> Draw
    ('r',s,r)   -> Discard (convertCard s r)

--Reads input
readMoves::[Move]-> IO [Move]
readMoves moves = do
    line <- getLine
    if line == "." then return moves
        else if line == "d" then readMoves ((convertMove (line!!0) 's' 'r') : moves)
            else if length line == 3 then readMoves ((convertMove (line!!0) (line!!1) (line!!2)):moves)
                else error "Move input is not in correct format."


