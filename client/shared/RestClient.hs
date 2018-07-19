{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module RestClient
  (
#ifdef __GHCJS__
    getChatHistory
  , getRooms
  , postRoom
  , deleteRoom
  , login
#endif
  ) where

import           Data.Proxy
import           Data.Text                         (Text)
import           Miso.String
import           Servant.API
#ifdef __GHCJS__
import           Servant.Client.Ghcjs
import           Servant.Client.Internal.XhrClient (runClientMOrigin)
#endif

import qualified Businesstypes                     as BT

type GetChatHistoryAPI = "chatRoom" :> Capture "rid" Int :> Get '[JSON] BT.ChatMessageLog
type GetRoomsAPI       = "chatRoom" :> Get '[JSON] [BT.ChatRoom]
type PostRoomAPI       = "chatRoom" :> ReqBody '[JSON] BT.ChatRoom :> Post '[JSON] BT.Id
type DeleteRoomAPI     = "chatRoom" :> Capture "rid" Int :> Delete '[JSON] BT.Id
type LoginAPI          = "participant" :> Capture "participantName" Text :> Get '[JSON] BT.Participant

#ifdef __GHCJS__
chatServer :: ClientEnv
chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

getChatHistoryREST :: Client ClientM GetChatHistoryAPI
getChatHistoryREST = client (Proxy @GetChatHistoryAPI)

getChatHistory :: BT.Id -> IO (Either ServantError BT.ChatMessageLog)
getChatHistory (BT.Id rid) = runClientMOrigin (getChatHistoryREST (read $ unpack rid)) chatServer

getRoomsREST :: Client ClientM GetRoomsAPI
getRoomsREST = client (Proxy @GetRoomsAPI)

getRooms :: IO (Either ServantError [BT.ChatRoom])
getRooms = runClientMOrigin getRoomsREST chatServer

postRoomREST :: Client ClientM PostRoomAPI
postRoomREST = client (Proxy @PostRoomAPI)

postRoom :: BT.ChatRoom -> IO (Either ServantError BT.Id)
postRoom cr = runClientMOrigin (postRoomREST cr) chatServer

deleteRoomREST :: Client ClientM DeleteRoomAPI
deleteRoomREST = client (Proxy @DeleteRoomAPI)

deleteRoom :: BT.Id -> IO (Either ServantError BT.Id)
deleteRoom (BT.Id rid) = runClientMOrigin (deleteRoomREST (read $ unpack rid)) chatServer

loginREST :: Client ClientM LoginAPI
loginREST = client (Proxy @LoginAPI)

login :: MisoString -> IO (Either ServantError BT.Participant)
login pn = runClientMOrigin (loginREST (fromMisoString pn)) chatServer
#endif
