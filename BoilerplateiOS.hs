{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module BoilerplateiOS where

import Boilerplate

import ObjectiveHaskellMini.ObjC
import ObjectiveHaskellMini.NSArray
import ObjectiveHaskellMini.NSNumber
import ObjectiveHaskellMini.NSDictionary ()

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable as F
import Control.Applicative
import Control.Monad

-- Foreign Exports

foreign export ccall "simStep" simStep :: UnsafeId -> IO UnsafeId
simStep :: UnsafeId -> IO UnsafeId
simStep uSimId = do
    sim <- fromObjC =<< retainedId uSimId
    autorelease =<< toObjC (step sim)

foreign export ccall "simPressure" simPressure :: UnsafeId -> IO UnsafeId
simPressure :: UnsafeId -> IO UnsafeId
simPressure uSimId = do
    sim <- fromObjC =<< retainedId uSimId
    autorelease =<< toObjC (Map.map toRational $ getPressure sim)

foreign export ccall "simSet" simSet :: UnsafeId -> UnsafeId -> UnsafeId -> IO UnsafeId
simSet :: UnsafeId -> UnsafeId -> UnsafeId -> IO UnsafeId
simSet uPointId uCellId uSimId = do
    point   <- fromObjC =<< retainedId uPointId
    cell    <- fromObjC =<< retainedId uCellId
    sim     <- fromObjC =<< retainedId uSimId
    autorelease =<< toObjC (setCell point cell sim)

-- Bridged instances

instance Bridged Point where
    toObjC Point{..} = nsArrayFromIntPair (ptX, ptY)
    fromObjC pointId = do
        (x, y) <- intPairFromNSArray pointId
        return Point { ptX=x, ptY=y }

instance Bridged Force where
    toObjC Force{..} = nsArrayFromIntPair (frX, frY)
    fromObjC forceId = do
        (x, y) <- intPairFromNSArray forceId
        return Force { frX=x, frY=y }

instance Bridged Cell where
    toObjC cell = toObjC (toRational $ fromEnum cell)
    fromObjC cellId = toEnum . floor <$> (fromObjC cellId :: IO Rational)

instance Bridged Simulation where
    toObjC Simulation{..} = toObjC simGrid
    fromObjC simId = do
        simMap <- fromObjC simId
        return . makeSimulation . Map.toList $ simMap

nsArrayFromIntPair :: (Integer, Integer) -> IO Id
nsArrayFromIntPair (x, y) = do
    xId <- toObjC (toRational x)
    yId <- toObjC (toRational y)
    toObjC (Seq.fromList [xId, yId])

intPairFromNSArray :: Id -> IO (Integer, Integer)
intPairFromNSArray arrayId = do
    [xId, yId] <- toList <$> fromNSArray arrayId
    x <- floor <$> fromNSNumber xId
    y <- floor <$> fromNSNumber yId
    return (x, y)