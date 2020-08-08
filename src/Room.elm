module Room exposing (Room, decodeRoom, decodeRoomsList)

-- import Json.Encode
import Json.Decode
-- import Dict

type alias Room =
  { current_players : Int
  , max_players : Int
  , room_name : String
  , game_name : String
  }

  
-- encodeRoom : Room -> Json.Encode.Value
-- encodeRoom room =
--   Json.Encode.object [ ("roomname", Json.Encode.string room.roomname),
--                        ("nickname", Json.Encode.string room.nickname),
--                        ("color", Json.Encode.string room.color),
--                        ("score", Json.Encode.int room.score),
--                        ("is_admin", Json.Encode.bool room.is_admin),
--                        ("is_muted", Json.Encode.bool room.is_muted) ]

decodeRoom : Json.Decode.Decoder Room
decodeRoom =
  Json.Decode.map4
    Room
    (Json.Decode.field "current_players" Json.Decode.int)
    (Json.Decode.field "max_players" Json.Decode.int)
    (Json.Decode.field "room_name" Json.Decode.string)
    (Json.Decode.field "game_name" Json.Decode.string)
  

decodeRoomsList : Json.Decode.Decoder (List Room)
decodeRoomsList =
  Json.Decode.list decodeRoom
  