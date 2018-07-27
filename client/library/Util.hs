module Util
  ( mapEff
  )
where

import           Miso hiding (model)

mapEff
  :: (sa -> sm -> Effect sa sm)
  -> sa
  -> sm
  -> (sa -> a)
  -> (sm -> m)
  -> Effect a m
mapEff updater action model actionWrapper modelWrapper  =
  let (Effect rm ra) = updater action model
      newModel       = modelWrapper rm
      newAction      = fmap (mapSub actionWrapper) ra
  in  Effect newModel newAction
