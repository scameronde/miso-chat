{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module RestClient
  ( getChatHistory
  , getRooms
  , postRoom
  , deleteRoom
  , login
  )
where

import           Data.Proxy
import           Data.Text                         (Text)
import           Miso.String
import           Servant.API
import           Servant.Client.Ghcjs
import           Servant.Client.Internal.XhrClient (runClientMOrigin)

import Businesstypes.ChatRoom as ChatRoom
import Businesstypes.Id as Id
import Businesstypes.ChatMessageLog as ChatMessageLog
import Businesstypes.Participant as Participant

type GetChatHistoryAPI = "chatRoom" :> Capture "id" Int :> Get '[JSON] ChatMessageLog.ChatMessageLog
type GetRoomsAPI       = "chatRoom" :> Get '[JSON] [ChatRoom.ChatRoom]
type PostRoomAPI       = "chatRoom" :> ReqBody '[JSON] ChatRoom.ChatRoom :> Post '[JSON] Id.Id
type DeleteRoomAPI     = "chatRoom" :> Capture "id" Int :> Delete '[JSON] Id.Id
type LoginAPI          = "participant" :> Capture "participantName" Text :> Get '[JSON] Participant.Participant

chatServer :: ClientEnv
chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

getChatHistoryREST :: Client ClientM GetChatHistoryAPI
getChatHistoryREST = client (Proxy @GetChatHistoryAPI)

getChatHistory :: Id.Id -> IO (Either ServantError ChatMessageLog.ChatMessageLog)
getChatHistory (Id.Id rid) =
  runClientMOrigin (getChatHistoryREST (read $ unpack rid)) chatServer

getRoomsREST :: Client ClientM GetRoomsAPI
getRoomsREST = client (Proxy @GetRoomsAPI)

getRooms :: IO (Either ServantError [ChatRoom.ChatRoom])
getRooms = runClientMOrigin getRoomsREST chatServer

postRoomREST :: Client ClientM PostRoomAPI
postRoomREST = client (Proxy @PostRoomAPI)

postRoom :: ChatRoom.ChatRoom -> IO (Either ServantError Id.Id)
postRoom cr = runClientMOrigin (postRoomREST cr) chatServer

deleteRoomREST :: Client ClientM DeleteRoomAPI
deleteRoomREST = client (Proxy @DeleteRoomAPI)

deleteRoom :: Id.Id -> IO (Either ServantError Id.Id)
deleteRoom (Id.Id rid) =
  runClientMOrigin (deleteRoomREST (read $ unpack rid)) chatServer

loginREST :: Client ClientM LoginAPI
loginREST = client (Proxy @LoginAPI)

login :: MisoString -> IO (Either ServantError Participant.Participant)
login pn = runClientMOrigin (loginREST (fromMisoString pn)) chatServer
