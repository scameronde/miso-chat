{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
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
import           Data.Text                      ( Text )
import           Miso.String
import           Servant.API
import           Servant.Client.Ghcjs
import           Servant.Client.Internal.XhrClient
                                                ( runClientMOrigin )

import           Businesstypes.ChatRoom         ( ChatRoom )
import           Businesstypes.Id               ( Id(Id) )
import           Businesstypes.ChatMessageLog   ( ChatMessageLog )
import           Businesstypes.Participant      ( Participant )

type GetChatHistoryAPI = "chatRoom" :> Capture "id" Int :> Get '[JSON] ChatMessageLog
type GetRoomsAPI       = "chatRoom" :> Get '[JSON] [ChatRoom]
type PostRoomAPI       = "chatRoom" :> ReqBody '[JSON] ChatRoom :> Post '[JSON] Id
type DeleteRoomAPI     = "chatRoom" :> Capture "id" Int :> Delete '[JSON] Id
type LoginAPI          = "participant" :> Capture "participantName" Text :> Get '[JSON] Participant

chatServer :: ClientEnv
chatServer = ClientEnv (BaseUrl Http "localhost" 4567 "")

getChatHistoryREST :: Client ClientM GetChatHistoryAPI
getChatHistoryREST = client (Proxy @GetChatHistoryAPI)

getChatHistory :: Id -> IO (Either ServantError ChatMessageLog)
getChatHistory (Id rid) =
  runClientMOrigin (getChatHistoryREST (read $ unpack rid)) chatServer

getRoomsREST :: Client ClientM GetRoomsAPI
getRoomsREST = client (Proxy @GetRoomsAPI)

getRooms :: IO (Either ServantError [ChatRoom])
getRooms = runClientMOrigin getRoomsREST chatServer

postRoomREST :: Client ClientM PostRoomAPI
postRoomREST = client (Proxy @PostRoomAPI)

postRoom :: ChatRoom -> IO (Either ServantError Id)
postRoom cr = runClientMOrigin (postRoomREST cr) chatServer

deleteRoomREST :: Client ClientM DeleteRoomAPI
deleteRoomREST = client (Proxy @DeleteRoomAPI)

deleteRoom :: Id -> IO (Either ServantError Id)
deleteRoom (Id rid) =
  runClientMOrigin (deleteRoomREST (read $ unpack rid)) chatServer

loginREST :: Client ClientM LoginAPI
loginREST = client (Proxy @LoginAPI)

login :: MisoString -> IO (Either ServantError Participant)
login pn = runClientMOrigin (loginREST (fromMisoString pn)) chatServer
