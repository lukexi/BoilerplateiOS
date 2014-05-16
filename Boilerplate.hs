{-# LANGUAGE RecordWildCards, FlexibleContexts, PackageImports, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Boilerplate where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Data.Maybe
import "mtl" Control.Monad.State
import Control.Applicative
import Data.Monoid
--import Debug.Trace

data Cardinal = North | South | East | West deriving (Eq, Show, Ord, Enum)

movePoint :: Cardinal -> Point -> Point
movePoint North point = point { ptY = succ . ptY $ point}
movePoint South point = point { ptY = pred . ptY $ point}
movePoint East  point = point { ptX = succ . ptX $ point}
movePoint West  point = point { ptX = pred . ptX $ point}

addForce :: Cardinal -> Integer -> Force -> Force
addForce North direction force = force { frY = frY force + direction }
addForce South direction force = force { frY = frY force + direction * (-1) }
addForce East  direction force = force { frX = frX force + direction }
addForce West  direction force = force { frX = frX force + direction * (-1) }

data Cell = Ground 
          | Nothingness 
          | Shuttle 
          | ThinShuttle 
          | ThinSolid 
          | Bridge 
          | Positive 
          | Negative 
          deriving (Eq, Show, Ord, Enum)
data Point = Point {ptX::Integer, ptY::Integer} deriving (Eq, Show, Ord)
data Force = Force {frX::Integer, frY::Integer} deriving (Eq, Show, Ord)
data Simulation = Simulation {simGrid::Map Point Cell, simEngines::Set Point} deriving (Show)

isEngine, isMovable, isPermeable, isShuttle, isShuttleOrThinShuttle, isBridge :: Cell -> Bool
isEngine               = (`elem` [Positive, Negative])
isMovable              = (`elem` [Nothingness, Shuttle, ThinShuttle])
isPermeable            = (`elem` [Nothingness, ThinShuttle, ThinSolid])
isShuttleOrThinShuttle = (`elem` [Shuttle, ThinShuttle])
isShuttle              = (== Shuttle)
isBridge               = (== Bridge)

type FillState = (Set Point, [Point])

visit :: (MonadState FillState m) => Point -> m ()
visit point = do
    (visited, toExplore) <- get
    unless (Set.member point visited) $ 
        put (Set.insert point visited, toExplore ++ [point])

fill :: Monad m => Point
                -> (Point -> StateT FillState m Bool)
                -> m ()
fill initialSquare f = do
    _ <- runStateT go (Set.singleton initialSquare, [initialSquare])
    return ()
    where
        go = do
            (visited, toExplore) <- get
            case toExplore of
                [] -> return ()
                (p:ps) -> do
                    put (visited, ps)
                    ok <- f p
                    when ok $ forM_ [North .. ] $ \dir -> visit (movePoint dir p)
                    go

setCell :: Point -> Maybe Cell -> Simulation -> Simulation
setCell point maybeCell Simulation{..} = case maybeCell of
    Just cell -> Simulation 
        { simGrid = Map.insert point cell simGrid
        , simEngines = if isEngine cell then Set.insert point simEngines else simEngines
        }
    Nothing -> Simulation 
        { simGrid = Map.delete point simGrid
        , simEngines = case Map.lookup point simGrid of 
            Just cell | isEngine cell -> Set.delete point simEngines
            _                         -> simEngines
        }

cellLookup :: Point -> Simulation -> Cell
cellLookup point = fromMaybe Ground . Map.lookup point . simGrid

tryMove :: Simulation -> [Point] -> Cardinal -> Maybe Simulation
tryMove sim@Simulation{..} points direction 
    | not $ all (isMovable . flip cellLookup sim . movePoint direction) points = Nothing
    | otherwise = let 
        (shuttle, sim') = foldl' (\(shuttleSoFar, simSoFar) point -> 
                let newShuttle = Map.insert point (cellLookup point simSoFar) shuttleSoFar
                    newSim = setCell point (Just Nothingness) simSoFar
                in (newShuttle, newSim)
            ) (Map.empty, sim) points
        in Just $ foldl' (\simSoFar point -> 
                setCell (movePoint direction point) (Map.lookup point shuttle) simSoFar
            ) sim' points

engineDirection :: Point -> Map Point Cell -> Integer
engineDirection point grid = if Map.lookup point grid == Just Positive then 1 else -1

getPressure :: Simulation -> Map Point Integer
getPressure sim@Simulation{..} = foldl' getPressure' mempty (Set.toList simEngines)
    where getPressure' pressureSoFar point = execState (doFill point) pressureSoFar
          doFill point = fill point $ \fillPoint -> do
            let cell | fillPoint == point = Nothingness
                     | otherwise          = cellLookup fillPoint sim
            if isPermeable cell
                then do
                    let addPressure atPoint = lift . modify $ \pressures -> 
                            let oldPressure = fromMaybe 0 (Map.lookup fillPoint pressures)
                                direction = engineDirection point simGrid
                            in Map.insert atPoint (oldPressure + direction) pressures
                    addPressure fillPoint
                    forM_ [North ..] $ \dir -> do
                        let go atPoint = do
                                let newPoint = movePoint dir atPoint
                                if Map.lookup newPoint simGrid == Just Bridge
                                    then do
                                        addPressure newPoint
                                        go newPoint
                                    else return newPoint
                        lastPoint <- go fillPoint
                        when (checkIf lastPoint isPermeable simGrid) $ 
                            visit lastPoint
                    return True
                else return False

checkIf :: Point -> (a -> Bool) -> Map Point a -> Bool
checkIf point predicate grid = (predicate <$> Map.lookup point grid) == Just True

data AShuttle = AShuttle {shPoints::[Point], shForce::Force} deriving (Eq, Show, Ord)
data SimState = SimState 
    { ssShuttleMap :: Map Point AShuttle
    , ssShuttles   :: Set AShuttle
    , ssSimulation :: Simulation
    } deriving (Show)

makeShuttle :: AShuttle
makeShuttle = AShuttle {shPoints=[], shForce=Force 0 0}

getShuttle :: (MonadState SimState m) => Point -> m (Maybe AShuttle)
getShuttle point = do
    grid <-  gets $ simGrid . ssSimulation
    case Map.lookup point grid of
        Just s | isShuttle s -> do
            maybeShuttleInMap <- gets (Map.lookup point . ssShuttleMap)
            case maybeShuttleInMap of
                Just shuttleInMap -> return (Just shuttleInMap)
                Nothing -> do
                    shuttlePoints <- flip execStateT [] $ fill point $ \fillPoint -> 
                            if checkIf fillPoint isShuttleOrThinShuttle grid 
                                then do
                                    lift $ modify (fillPoint:)
                                    return True
                                else return False
                    let shuttle = makeShuttle{shPoints=shuttlePoints}
                        mappedPoints = Map.fromList (zip shuttlePoints (repeat shuttle))
                    modify $ \simState@SimState{..} ->
                        simState{ssShuttleMap=ssShuttleMap <> mappedPoints}
                    return $ Just shuttle
        _ -> return Nothing

addShuttle :: MonadState SimState m => AShuttle -> m ()
addShuttle shuttle@AShuttle{..} = do
    let mappedPoints = Map.fromList (zip shPoints (repeat shuttle))
    modify $ \simState@SimState{..} ->
        simState { ssShuttleMap=ssShuttleMap <> mappedPoints
                 , ssShuttles=Set.insert shuttle ssShuttles
                 }
removeShuttle :: MonadState SimState m => AShuttle -> m ()
removeShuttle shuttle@AShuttle{..} = 
    modify $ \simState@SimState{..} ->
        simState { ssShuttleMap=foldl' (flip Map.delete) ssShuttleMap shPoints
                 , ssShuttles=Set.delete shuttle ssShuttles
                 }

replaceShuttle :: MonadState SimState m 
              => AShuttle -> AShuttle -> m ()
replaceShuttle shuttle newShuttle = do
    removeShuttle shuttle
    addShuttle newShuttle

vertDirection :: Force -> Maybe Cardinal
vertDirection Force{..}
    | frY > 0   = Just North
    | frY < 0   = Just South
    | otherwise = Nothing

horizDirection :: Force -> Maybe Cardinal
horizDirection Force{..}
    | frX > 0   = Just East
    | frX < 0   = Just West
    | otherwise = Nothing

step :: Simulation -> Simulation
step sim = ssSimulation $ flip execState (SimState mempty mempty sim) $ do
    let grid = simGrid sim
    forM_ (Set.toList $ simEngines sim) $ \enginePoint -> do
        let direction = engineDirection enginePoint grid
        fill enginePoint $ \fillPoint -> do
            let cell | fillPoint == enginePoint = Nothingness
                     | otherwise                = cellLookup fillPoint sim
            if isPermeable cell 
                then do
                    forM_ [North ..] $ \cardinal -> do
                        let movedPoint = movePoint cardinal fillPoint
                            moveShuttleOrElse atPoint orElse = do
                                mShuttle <- lift $ getShuttle atPoint
                                case mShuttle of
                                    Just shuttle@AShuttle{..} -> 
                                        lift $ replaceShuttle shuttle 
                                            (shuttle {shForce=addForce cardinal direction shForce})
                                    Nothing -> orElse

                        moveShuttleOrElse movedPoint $ when (checkIf movedPoint isBridge grid) $ do
                            let movedPoint' = movePoint cardinal movedPoint
                                go point = if checkIf point isBridge grid then go (movePoint cardinal point) else point
                                lastPoint = go movedPoint'
                            moveShuttleOrElse lastPoint $ when (checkIf lastPoint isPermeable grid) $ visit lastPoint
                    return True
                else return False

    shuttles <- gets (Set.toList . ssShuttles)
    forM_ shuttles $ \AShuttle{..} ->
        modify $ \simState@SimState{..} -> 
            case vertDirection shForce >>= tryMove ssSimulation shPoints of
                Just newSimulationVert -> simState{ssSimulation = newSimulationVert}
                Nothing -> case horizDirection shForce >>= tryMove ssSimulation shPoints of
                    Just newSimulationHoriz -> simState{ssSimulation = newSimulationHoriz}
                    Nothing -> simState

makeSimulation :: [(Point, Cell)] -> Simulation
makeSimulation cellPoints = Simulation
    { simGrid = Map.fromList cellPoints
    , simEngines = Set.fromList . map fst . filter (isEngine . snd) $ cellPoints
    }

showSim :: Simulation -> [Cell]
showSim Simulation{..} = map snd $ Map.toAscList simGrid

main :: IO ()
main = do
    putStrLn "Push a shuttle along a tunnel:"
    let sim1 = iterate step $ makeSimulation [(Point 0 0, Positive), (Point 1 0, Shuttle), (Point 2 0, Nothingness), (Point 3 0, Nothingness)]
    mapM_ (print . showSim) $ take 4 sim1

    putStrLn "\nSimple oscillator:"
    let sim2 = iterate step $ makeSimulation [(Point 0 0, Nothingness), (Point 0 1, Shuttle), (Point 1 0, Negative), (Point 1 1, Nothingness)]
    mapM_ (print . showSim) $ take 20 sim2
