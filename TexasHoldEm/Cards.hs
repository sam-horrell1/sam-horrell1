import System.Random (randoms, mkStdGen, randomRIO, randomIO)
import Data.List ( group, sort, sortBy, subsequences, maximumBy, elemIndex)
import Data.Ord (comparing)

data Suit = Clubs | Spades | Hearts | Diamonds
    deriving (Show, Enum, Eq, Ord, Bounded)

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Enum, Eq, Ord, Bounded)

data Card = Card {suit :: Suit, cardValue :: CardValue}
    deriving (Show, Eq, Ord)

type Deck = [Card]


data Player = Player {
    playerName :: String,
    playerChips :: Int,
    playerHand :: Deck,
    isDealer :: Bool,
    strategy :: Strategy
} deriving (Show, Eq)

type Players = [Player]


data GameState = GameState {
    activePlayers :: Players,
    allPlayers :: Players,
    deck :: Deck,
    comCards :: Deck,
    pot :: Int,
    bets :: [(String, Int)],
    dealerPos :: Int,
    sBlindPos :: Int,
    bBlindPos :: Int
} deriving (Show)


data Hand = Null | HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
    deriving (Enum, Eq, Show, Ord)

data Strategy = Passive | Aggressive | Random
    deriving (Show, Eq)

data Action = Fold | Call | Raise
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------------
-- STEP ONE

-- Creates a player and returns it
createPlayer :: String -> Int -> Strategy -> Player
createPlayer name chips strat =
    Player {playerName = name, playerChips = chips,
            playerHand = [], isDealer = False, strategy = strat}

-- Creates players with passed in names and strategies
createPlayers :: [(String, Strategy)] -> Players
createPlayers info =
    [ createPlayer name 500 strategy | (name, strategy) <- info ]

-- Selects first n cards from deck
dealCards :: Int -> Deck -> (Deck, Deck)
dealCards = splitAt

-- Deals two cards to each player recursively until all have been dealt
dealToPlayers :: Players -> Deck -> (Players, Deck)
dealToPlayers [] deck = ([], deck)
dealToPlayers (player:players) deck =
    let (hand, remainingDeck) = dealCards 2 deck
        updatedPlayer = player { playerHand = hand }
        (dealtPlayers, finalDeck) = dealToPlayers players remainingDeck
    in (updatedPlayer:dealtPlayers, finalDeck)

-- Selects n community cards, updates the gameState
dealComCards :: GameState -> Int -> GameState
dealComCards gameState num =
    let gameDeck = deck gameState
        (dealtCards, remainingDeck) = dealCards num gameDeck
        updatedComCards = comCards gameState ++ dealtCards
    in gameState { comCards = updatedComCards, deck = remainingDeck }

-- From lecture slides
-- Shuffles deck by sorting by giving each card a random number and sorting by it
-- Added a seed so deck isn't shuffled the same each time
cmp :: (a, Int) -> (a, Int) -> Ordering
cmp (_, y1) (_, y2) = compare y1 y2

shuffleDeck :: Deck -> Int -> Deck
shuffleDeck deck seed = [card | (card, _) <- sortBy cmp (zip deck randomNumbers)]
    where
        randomNumbers = randoms (mkStdGen seed) :: [Int]

-- ---------------------------------------------------------------------------------
-- STEP TWO

-- Returns list of cardValues of cards
cardValues :: [Card] -> [CardValue]
cardValues = map cardValue

-- Returns list of suits of cards
cardSuits :: [Card] -> [Suit]
cardSuits = map suit

-- Generates all combinations of cards, filters so it only uses combinations with a length of 5
choose5Cards :: Deck -> [Deck]
choose5Cards cards =
    let
        combinations = subsequences cards
    in filter (\hand -> length hand == 5) combinations

-- Starting with the best ranks, checks if a set of cards fits a rank
-- If it does, return that hand and the cards that made it up
classifyHand :: Deck -> (Hand, Deck)
classifyHand cards
    | isRoyalFlush cards = (RoyalFlush, cards)
    | isStraightFlush cards = (StraightFlush, cards)
    | isFourOfKind cards = (FourOfAKind, cards)
    | isFullHouse cards = (FullHouse, cards)
    | isFlush cards = (Flush, cards)
    | isStraight cards = (Straight, cards)
    | isThreeOfKind cards = (ThreeOfAKind, cards)
    | isTwoPair cards = (TwoPair, cards)
    | isOnePair cards = (OnePair, cards)
    | otherwise = (HighCard, cards)

-- Finds the highest hand rank, and the cards with that rank
-- Sorts and groups cardValues by value, and then by length
-- (If there is a pair or a three of a kind those cards will be at the front of the list)
-- [Jack, Ace, Two, Five, Two] -> [[Two, Two], [Ace], [Jack], [Five]]
-- Uses tieBreak to find the best set of cards e.g. pair of 4s beats pair of 2s
evaluateHand :: Deck -> (Hand, [[CardValue]])
evaluateHand cards =
    let results = map classifyHand (choose5Cards cards)

        highestRank = maximum (map fst results)
        highestRankCards = [handCards | (hand, handCards) <- results, hand == highestRank]

        groupedCardValues = map (group . sort . cardValues) highestRankCards
        sortedGroupedValues = map (reverse . sortBy (comparing length)) groupedCardValues

        bestCardGroup = tieBreak sortedGroupedValues
    in (highestRank, bestCardGroup)

-- ---------------------------------------------------------------------------

-- Check if the hand has one pair
-- It groups cards by their value, and checks if this formed pairs
isOnePair :: Deck -> Bool
isOnePair cards =
    let groupedCards = group (sort (cardValues cards))
        pairs = filter (\group -> length group == 2) groupedCards
    in length pairs == 1

-- Check if the hand has two pairs
isTwoPair :: Deck -> Bool
isTwoPair cards =
    let groupedCards = group (sort (cardValues cards))
        pairs = filter (\group -> length group == 2) groupedCards
    in length pairs == 2

-- Check if hand has a trio
isThreeOfKind :: Deck -> Bool
isThreeOfKind cards =
    let groupedCards = group (sort (cardValues cards))
        threes = filter (\group -> length group == 3) groupedCards
    in length threes == 1

-- Checks there's no duplicates (length of all lists is 1) and the range is 4
-- If this is the case, it must be a straight
isStraight :: Deck -> Bool
isStraight cards =
    let sortedCards = sort (cardValues cards)
        noDupes = all (\group -> length group == 1) (group sortedCards)
        range = fromEnum (last sortedCards) - fromEnum (head sortedCards)
    in noDupes && range == 4

-- Checks cards are all same suit 
isFlush :: Deck -> Bool
isFlush cards =
    let groupedSuits = group (sort (cardSuits cards))
        fives = filter (\group -> length group == 5) groupedSuits
    in length fives == 1

-- Checks cards have a trio and a pair
isFullHouse :: Deck -> Bool
isFullHouse cards =
    let groupedCards = group (sort (cardValues cards))
        threes = filter (\group -> length group == 3) groupedCards
        twos = filter (\group -> length group == 2) groupedCards
    in length threes == 1 && length twos == 1

-- Checks cards have a quad
isFourOfKind :: Deck -> Bool
isFourOfKind cards =
    let groupedCards = group (sort (cardValues cards))
        fours = filter (\group -> length group == 4) groupedCards
    in length fours == 1

-- Checks if hand is a flush and straight
isStraightFlush :: Deck -> Bool
isStraightFlush cards = isFlush cards && isStraight cards

-- Checks if hand is a straight flush and has an ace
-- If it has an ace and is a straight flush, it must be a royal flush
isRoyalFlush :: Deck -> Bool
isRoyalFlush cards =
    let sortedCards = sort (cardValues cards)
        hasAce = last sortedCards == Ace
    in hasAce && isStraightFlush cards

-- ----------------------------------------------------------------------------
-- STEP THREE

-- Removes player from activePlayers if they're folding
foldPlayer :: GameState -> String -> GameState
foldPlayer state name =
    let
        updatedPlayers = filter (\player -> playerName player /= name) (activePlayers state)
    in state {activePlayers = updatedPlayers}

-- If a player is betting money, it lowers their balance
-- If a player is gaining money (from winning), it increases their balance
-- Updates allPlayers as well (reason explained in report)
adjustBalance :: GameState -> String -> Int -> GameState
adjustBalance state name amount =
    let
        updatedActPlayers = map (\p -> if playerName p == name
                                     then p { playerChips = playerChips p + amount }
                                     else p) (activePlayers state)

        updatedAllPlayers = map (\p -> if playerName p == name
                                     then p { playerChips = playerChips p + amount }
                                     else p) (allPlayers state)
    in state { activePlayers = updatedActPlayers, allPlayers = updatedAllPlayers }

-- Adds a bet to the list of bets, deducts user's balance and increases pot
-- If the betAmount is greater than the player's chips, they can't play, so they fold
-- betAmount is the last bet, plus whatever they are raising by (0 if they are calling)
callOrRaise :: GameState -> Player -> Int -> GameState
callOrRaise state player amount =
    let
        currentBets = bets state
        betAmount = if null currentBets
            then amount
            else snd (last currentBets) + amount

        updatedState = if betAmount > playerChips player
            then foldPlayer state (playerName player)
            else let
                    updatedBalance = adjustBalance state (playerName player) (- betAmount)
                    updatedBets = currentBets ++ [(playerName player, betAmount)]
                    updatedPot = pot state + betAmount

                in updatedBalance {bets = updatedBets, pot = updatedPot}
    in updatedState


-- For each player, allows them to fold, call or raise
-- Recurs until each player has been looped through
-- If only one player's left in the game, stop looping
-- If a player raises, allows each active player to respond to it
bettingRound :: GameState -> Players -> IO GameState
bettingRound state [] = return state
bettingRound state (player:players) = do

    (updatedState, action) <- case strategy player of
        Passive -> passiveAction state player
        Aggressive -> aggressiveAction state player
        Random -> randomAction state player

    putStrLn ("        " ++ playerName player ++ " has chosen to " ++ show action)

    if length (activePlayers updatedState) <= 1
        then return updatedState
        else do
            if action == Raise
                then do

                    -- Finds index of the player that raised
                    -- If it finds player, it allows all other active players to respond
                    -- Rotates list so player after raiser goes first, removes player that raised
                    -- If it can't find player, they tried to raise but didn't have the chips folded
                    -- Therefore just go to the next player (like you would if they wanted to fold)
                    let playerNames = map playerName (activePlayers updatedState)
                        playerIndex = elemIndex (playerName player) playerNames

                    case playerIndex of
                        Just playerIndex -> do
                            let rotatedState = rotateList updatedState playerIndex
                                lastRemoved = init (activePlayers rotatedState)
                            bettingRound rotatedState lastRemoved
                        Nothing -> do
                            bettingRound updatedState players

            else bettingRound updatedState players

-- Deals community cards for the round and runs bettingRound
-- Loop finishes when cardNums is iterated through
-- Or when there's a winner
game :: GameState -> [Int] -> IO GameState
game state [] = return state
game state (num:cardNums) = do

    if length (activePlayers state) <= 1 
        then return state
        else do
            let stateAfterComCardsDeal = dealComCards state num

            putStrLn "    New Betting Round:"

            stateAfterBet <- bettingRound stateAfterComCardsDeal (activePlayers stateAfterComCardsDeal)
            let readyForNextBet = stateAfterBet { bets = [] }

            game readyForNextBet cardNums

-- First shuffles deck, deals players' cards, starts gameState, sorts dealer and blinds
-- Calls game, allowing betting rounds to be run
-- After game, determines and outputs winner
gameLoop :: Players -> Deck -> Int -> Int -> IO ()
gameLoop players deck dealer count = do

    putStrLn ("Round " ++ show count ++ " starting:")

    seed <- randomIO :: IO Int
    let shuffledDeck = shuffleDeck deck seed
        (dealtPlayers, remainingDeck) = dealToPlayers players shuffledDeck
        startingGameState = GameState {activePlayers = dealtPlayers, allPlayers = dealtPlayers,
                                        deck = remainingDeck, comCards = [], pot = 0, bets = [],
                                        dealerPos = dealer, sBlindPos = 0, bBlindPos = 0
                                        }
        stateRotatedDealer = rotateDealer startingGameState
        stateAfterBlinds = blinds stateRotatedDealer

    -- List passed in is how many community cards to reveal for each betting round
    updatedState <- game stateAfterBlinds [0, 3, 1, 1]

    -- Winner is either the remaining player or the winner of the showdown (determineWinner)
    -- Updates player's chips and empties the pot
    let
        winner = if length (activePlayers updatedState) <= 1
            then head (activePlayers updatedState)
            else determineWinner (activePlayers updatedState) (comCards updatedState)

        playerUpdated = adjustBalance updatedState (playerName winner) (pot updatedState)
        potUpdated = playerUpdated { pot = 0 }

        playersWithChips = filter (\player -> playerChips player > 0) (allPlayers potUpdated)

    putStrLn ("Winner of round " ++ show count ++ " is " ++ playerName winner ++ ", they have " ++ show (playerChips winner + pot updatedState))
    putStr "\n"

    -- If there's only one player with chips, they win
    -- If the game has gone on for 100 rounds, the winner is the one with the most chips
    -- Otherwise, loop again
    if length playersWithChips == 1
        then do
            let winner = head playersWithChips
            putStrLn ("Overall winner: " ++ playerName winner ++ " with " ++ show (playerChips winner) ++ " chips")
        else
            if count >= 100 then do
                let winner = maximumBy (comparing playerChips) players
                putStrLn ("Overall winner after 100 games: " ++ playerName winner ++ " with " ++ show (playerChips winner) ++ " chips")
            else
                gameLoop playersWithChips shuffledDeck (dealerPos updatedState) (count+1)


-- Creates tuple with the player, their hand and their card groups
-- Finds the highest hand rank, and gets the players and card groups that have that rank
-- Uses tie break to find the best set of cards
-- Finds the winner associated with these cards and returns them
-- If multiple players with same rank and exact same hand, winner is head of list
determineWinner :: Players -> Deck -> Player
determineWinner players communityCards =
    let playerHandEvals = [(player, evaluateHand (playerHand player ++ communityCards)) | player <- players]
        highestRank = maximum (map (fst . snd) playerHandEvals)
        possibleWinners = [(player, cardGroup) | (player, (hand, cardGroup)) <- playerHandEvals, hand == highestRank]

        groupedCardValues = map snd possibleWinners
        bestGroup = tieBreak groupedCardValues
        winner = head [player | (player, cards) <- possibleWinners, cards == bestGroup]
    in winner


-- Loops through two lists of cardValues and compares their elements 
-- Continues until one list has a different value in an index to the other
-- Returns true if the first list has a higher value, false if the second's higher
-- If the lists are the same, it just returns False
compareHand :: [[CardValue]] -> [[CardValue]] -> Bool
compareHand [] [] = False
compareHand (x:xs) (y:ys)
  | head x > head y = True
  | head y > head x = False
  | otherwise = compareHand xs ys

-- Takes a list of cardValues, and uses compareHand to find the best cards
-- Works with just one card value (tieBreak [hand] = hand)
-- Uses recursion to loop through every list inputted
tieBreak :: [[[CardValue]]] -> [[CardValue]]
tieBreak [] = error "No cards"
tieBreak [hand] = hand
tieBreak (hand:hands) =
    let bestHand = tieBreak hands
        winner = if compareHand hand bestHand
            then hand
            else bestHand
    in winner

-- Removes dealer status from current dealer, gives it to next in list, updates GameState
-- If dealer is last person in list, new dealer is the head of the list
rotateDealer :: GameState -> GameState
rotateDealer state =
    let
        dealerRemoved = changeDealerStatus state False

        newDealerPos = (dealerPos state + 1) `mod` length (activePlayers state)

        dealerPosUpdated = dealerRemoved { dealerPos = newDealerPos}
        dealerAdded = changeDealerStatus dealerPosUpdated True

    in dealerAdded

-- At dealerPos, it changes isDealer to true/false
-- Defines pos to make sure index not out of range
changeDealerStatus :: GameState -> Bool -> GameState
changeDealerStatus state change =
    let pos = dealerPos state `mod` length (activePlayers state)
        (left, right) = splitAt pos (activePlayers state)
        updatedPlayers = left ++ [ (head right) {isDealer = change}] ++ tail right
    in state { activePlayers = updatedPlayers }

-- Sorts the small and big blind bets, updates their position
-- Rotates list so blind players at back, players after blinds at front
blinds :: GameState -> GameState
blinds state =
    let
        (smallBlind, sBlindPos) = blind state 1
        (bigBlind, bBlindPos) = blind smallBlind 2
        stateAfterBlinds = bigBlind {sBlindPos = sBlindPos, bBlindPos = bBlindPos}
    in rotateList stateAfterBlinds bBlindPos

-- Determines position of blind, does their bet
-- blindPos is val in front of dealerPos (1 for small blind, 2 for big blind)
blind :: GameState -> Int -> (GameState, Int)
blind state val =
    let
        blindPos = (dealerPos state + val) `mod` length (activePlayers state)

        blindPlayer = activePlayers state !! blindPos
        stateAfterBet = callOrRaise state blindPlayer 10

    in (stateAfterBet, blindPos)

-- Rotates activePlayers so the player at an index is moved to the front
-- Used by blinds to move big blind to the back of the list, player after them to the front
-- Used after player raises to put player after raiser to the front
rotateList :: GameState -> Int -> GameState
rotateList state pos =
    let
        (left, right) = splitAt (pos + 1) (activePlayers state)
        newActPlayers = right ++ left
    in state {activePlayers = newActPlayers}

-- ----------------------------------------------------------------------------
-- STEP FOUR

-- Randomly folds, calls or raises
randomAction :: GameState -> Player -> IO (GameState, Action)
randomAction state player = do
    playerAction <- randomRIO (1, 3) :: IO Int
    let (updatedState, action) = case playerAction of
            1 -> (foldPlayer state (playerName player), Fold)
            2 -> (callOrRaise state player 0, Call)
            3 -> (callOrRaise state player 40, Raise)
    return (updatedState, action)

-- Uses random number to either fold or raise
passiveAction :: GameState -> Player -> IO (GameState, Action)
passiveAction state player = do
    playerAction <- randomRIO (1, 2) :: IO Int
    let (updatedState, action) = case playerAction of
            1 -> (foldPlayer state (playerName player), Fold)
            2 -> (callOrRaise state player 0, Call)
    return (updatedState, action)

-- Uses random number to fold, call or (80% of the time) raise
aggressiveAction :: GameState -> Player -> IO (GameState, Action)
aggressiveAction state player = do
    playerAction <- randomRIO (1, 10) :: IO Int
    let (updatedState, action) = case playerAction of
            1 -> (foldPlayer state (playerName player), Fold)
            2 -> (callOrRaise state player 0, Call)
            _ -> (callOrRaise state player 40, Raise)
    return (updatedState, action)

-- ----------------------------------------------------------------------------

main :: IO()
main = do

    let fullDeck = [Card s cv | s <- [Clubs .. Diamonds], cv <- [Two .. Ace]]
    let players = createPlayers [("George", Aggressive), ("Matty", Passive), ("Josh", Random), ("Danny", Aggressive)]

    gameLoop players fullDeck 0 1



