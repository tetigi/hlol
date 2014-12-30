module Grouch.Simulation.PaperTrade where

import Grouch.Data
import Grouch.Games.Common
import Grouch.Bots.Blackjack
import Grouch.Games.Blackjack
import Grouch.Logging.Logger

import Data.Maybe
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad

-- TODO Extract the core functionality of the paper trader and make it so that you can plug in behaviours

type Odds = Double
type Value = Double
type PlayerId = Int

data Bet = Bet {
      betSide :: Side
    , betPlayer :: PlayerId
    , betOdds :: Odds -- as in 6 for 5 to 1, or 2 for even
    , betVal  :: Value
} deriving (Show, Read)

data Side = Lay | Back deriving (Show, Read)
type Return = Double

evalWin :: Bet -> Return
evalWin (Bet Lay _ _ _)  = 0
evalWin (Bet Back _ odds front) = odds * front

evalLoss :: Bet -> Return
evalLoss (Bet Lay _ odds front)  = (front / odds) + front
evalLoss (Bet Back _ _ _) = 0

-- | Given a game snapshot, tells if the player won or lost
evaluateBet :: GameSnapshot -> Bet -> Return
evaluateBet snapshot bet =
    if playerHand `beatsDealers` dealerHand then evalWin bet else evalLoss bet
    where
        hs = assembleGame snapshot
        pname = "Player " ++ show (betPlayer bet)
        dealerHand = handValue $ fromMaybe
            (error "No dealer in hand, need dealer to evaluate bet!") $
            lookup "Dealer" hs
        playerHand = handValue $ fromMaybe
            (error $ "Could not find for " ++ pname ++ ", cannot eval bet") $
            lookup pname hs


data PlayerState = PlayerState {
      playerPot :: Double
    , playerBets :: [Bet]
} deriving (Show, Read)

defaultState :: PlayerState
defaultState = PlayerState 1000.0 []

-- Will need to iterate over the game, keeping tracks of what bets I have made and making new bets as opportunities come up.
-- Once the game ends, I will need to evaluate all the bets into a return, and work out my new pot. Actions should be written
-- to a log that I can put on disk somewhere.

type Simulation = StateT PlayerState IO ()

simulate :: PlayerState -> IO PlayerState
simulate initialState = do
    grouchInfo "Grouch.Simulation.PaperTrade" "Starting simulation..."
    end <- execStateT simulateStep initialState
    grouchInfo "Grouch.Simulation.PaperTrade" $ "Simulation finished with state " ++ show end
    return end

evaluateBets :: GameSnapshot -> Simulation
evaluateBets snapshot = do
    pstate <- get
    let returns = map (evaluateBet snapshot) $ playerBets pstate
    lift $ grouchInfo "Grouch.Simulation.PaperTrade" $ "Final hands: " ++ show (asHandValues $ assembleGame snapshot)
    lift $ grouchInfo "Grouch.Simulation.PaperTrade" $ "Winning bets: " ++ show (map snd $ filter (( > 0) . fst) $ zip returns $ playerBets pstate)
    put $ pstate { playerBets = [], playerPot = playerPot pstate + sum returns }

placeBet :: Bet -> Simulation
placeBet bet = do
    pstate <- get
    unless
        (betAlreadyPlaced (betPlayer bet) (playerBets pstate))
        (put $ pstate { playerBets = bet : playerBets pstate, playerPot = playerPot pstate - betVal bet })

betAlreadyPlaced :: PlayerId -> [Bet] -> Bool
betAlreadyPlaced i bs = i `elem` map betPlayer bs

getBestBack :: PlayerId -> GameSnapshot -> Maybe Price
getBestBack player snapshot = listToMaybe =<< lookup playerName playerBacks
    where
        playerName = "Player " ++ show player
        selections = marketSelections $ gameMarket snapshot
        playerBacks = map (selectionName &&& selectionBestBackPrices) selections

getBestLay :: PlayerId -> GameSnapshot -> Maybe Price
getBestLay player snapshot = listToMaybe =<< lookup playerName playerLays
    where
        playerName = "Player " ++ show player
        selections = marketSelections $ gameMarket snapshot
        playerLays = map (selectionName &&& selectionBestLayPrices) selections

getGoodBacks :: GameSnapshot -> [(PlayerId, Price, Double)]
getGoodBacks snapshot =
    map (\((name, actual), price) -> (playerId name, fromJust price, actual))
        $ filter (uncurry oddsAreBetter) $ zip bettingOdds bestBacks
    where
        bettingOdds = map (\x -> (fst x, probToOdds $ snd x)) $ fromMaybe [] probs
        probs = playerProbs $ assembleGame snapshot
        bestBacks = map (`getBestBack` snapshot) [1.. 4]
        playerId name = read [last name]

getGoodLays :: GameSnapshot -> [(PlayerId, Price, Double)]
getGoodLays snapshot =
    map (\((name, actual), price) -> (playerId name, fromJust price, actual))
        $ filter (uncurry oddsAreWorse) $ zip bettingOdds bestLays
    where
        bettingOdds = map (\x -> (fst x, probToOdds $ snd x)) $ fromMaybe [] probs
        probs = playerProbs $ assembleGame snapshot
        bestLays = map (`getBestLay` snapshot) [1.. 4]
        playerId name = read [last name]
probThreshold :: Double
probThreshold = 0.2

oddsAreBetter :: (String, Double) -> Maybe Price -> Bool
oddsAreBetter (_, odds) (Just price)
    | odds <= 2     = odds < (priceValue price - probThreshold)
    | otherwise     = False
oddsAreBetter _ _ = False

oddsAreWorse :: (String, Double) -> Maybe Price -> Bool
oddsAreWorse (_, odds) (Just price)
    | odds >= 3     = odds > (priceValue price + probThreshold)
    | otherwise     = False
oddsAreWorse _ _ = False

{-
oddsAreBetter :: (String, Double) -> Maybe Price -> Bool
oddsAreBetter (_, odds) (Just price) = odds < priceValue price
oddsAreBetter _ _ = False

oddsAreWorse :: (String, Double) -> Maybe Price -> Bool
oddsAreWorse (_, odds) (Just price) = odds > priceValue price
oddsAreWorse _ _ = False
-}

constructDefaultBet :: Side -> (PlayerId, Price, Double) -> Bet
constructDefaultBet side (player, price, _) =
    Bet side player (priceValue price) (min (priceAmountUnmatched price) 10)

constructKellyBet :: Side -> Double -> (PlayerId, Price, Double) -> Bet
constructKellyBet side pot (player, price, actual) = let kellyBet = pot * abs (kellyFrac (oddsToProb actual) (priceValue price)) in
    Bet side player (priceValue price) (min (priceAmountUnmatched price) kellyBet)

simulateStep :: Simulation
simulateStep = do
    snapshot <- lift getSnapshotTurbo
    let hands = map (\(p, h) -> ([head p, last p], handValue h)) $ assembleGame snapshot
    lift $ grouchDebug "Grouch.Simulation.PaperTrade" $ "Received new snapshot: " ++ show hands
    lift $ threadDelay 4000000
    pstate <- get

    let lays = map (constructDefaultBet Lay) $ getGoodLays snapshot
    let backs = map (constructDefaultBet Back) $ getGoodBacks snapshot
    --let lays = map (constructKellyBet Lay $ playerPot pstate) $ getGoodLays snapshot
    --let backs = map (constructKellyBet Back $ playerPot pstate) $ getGoodBacks snapshot

    lift $ grouchInfo "Grouch.Simulation.PaperTrade" $ "Current state: " ++ show pstate

    unless (playerPot pstate <= 0.0) $
    --unless (gameOver snapshot) $
        --unless (null lays) (placeBet $ head lays) >>
        --unless (null backs) (placeBet $ head backs) >>
        mapM_ placeBet lays >>
        mapM_ placeBet backs >>
        when (gameOver snapshot) (evaluateBets snapshot) >>
        simulateStep

kellyFrac :: Double -> Double -> Double
kellyFrac p b = (p * b - 1) / (b - 1)
