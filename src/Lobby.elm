port module Lobby exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Room exposing (Room)
import User exposing (User, isMuted, isAdmin)
{-
import Coordinate exposing (..)
import Move exposing (Direction(..), Move)
import Board
import Color exposing (..)
import Robot exposing (Robot)
import Goal exposing (GoalSymbol(..), Goal)
import User exposing (User)
import Chat exposing (Chatline)
-}

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (style, type_, title, placeholder, value, class, id, attribute, href)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode
import Json.Decode
import Set exposing (Set)
import Time


-- MAIN

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias JSONMessage = 
  { action : String 
  , content : Json.Encode.Value
  }

type GameName
  = Canoe
  | Codenames
  | Homeworlds
  | RicochetRobots
  | Unknown


type alias Model =
  { debugString : String,
    nameInProgress : String,
    colorInProgress : String,
    roomNameInProgress : String,
    roomNumInProgress : String,
    roomGameInProgress : String,
    rooms : Maybe ( List (Room) ),
    sorting : (String, Bool),
    toggleModal : Bool,
    toggleOptions : Bool,
    toggleView : String,
    showFullRooms : Bool,
    showEmptyRooms : Bool,
    user : User,
    allGames : List ( String ),
    ignoredGames : Set ( String )
  }



init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "Initialized model."
    ""
    ""
    ""
    ""
    ""
    Nothing
    ("room_name", False)
    False
    False
    "rooms"
    True
    True
    (User "Uninitialized" "" "#555759" 0 0 False False)
    []
    Set.empty
  , Cmd.none )



-- UPDATE

type Msg
  = Ping Time.Posix
  | GetJSON Json.Encode.Value
  | CreateRoom
  | GetRedirect Json.Encode.Value
  | GetRoomsList Json.Encode.Value
  | GetUser Json.Encode.Value
  | SetName String
  | SetColor String
  | SetRoomName String
  | SetRoomMaxPlayers String
  | SetRoomGameName String
  | SetSorting String Bool
  | Refresh
  | UpdateSettings
  | ToggleModal
  | ToggleOptions
  | ToggleView String
  | ToggleShowFullRooms
  | ToggleShowEmptyRooms
  | SetIgnoredGames (Set ( String ))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Ping newTime ->
      ( model
      , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "ping"),
                        ( "content", Json.Encode.string "ping" ) ] ) )
      )

    GetJSON json ->
      case Json.Decode.decodeValue decodeJSON json of
        Ok {action, content} ->
          case action of
            "connect_to_server"   ->
              update (SetName "TODO") model
            "update_rooms"   ->
              update (GetRoomsList content) model
            "update_user" ->
              update (GetUser content) model
            "redirect" ->
              update (GetRedirect content) model
            "ping" ->
              ( model, Cmd.none )

            _ ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | debugString = ("Bad JSON: " ++ (Json.Encode.encode 0 json))}, Cmd.none )

    CreateRoom ->
      ( model
      , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "create_room"),
                        ( "content", Json.Encode.object [ ("room_name", Json.Encode.string model.roomNameInProgress),
                                                          ("game_name", Json.Encode.string model.roomGameInProgress),
                                                          ("player_limit", Json.Encode.string model.roomNumInProgress) ] ) ] ) ) )
        -- url = getURL (getGameName model.roomGameInProgress) ++ model.roomNameInProgress

    GetRedirect json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok url ->
          -- ( { model | debugString = "URL: " ++ url }, Cmd.none )
          (model , Nav.load url )
        Err _ ->
          ( { model | debugString = "Error parsing userlist JSON"}, Cmd.none )

          
    GetRoomsList json ->
      case Json.Decode.decodeValue Room.decodeRoomsList json of
        Ok roomsList ->
          ( { model | debugString = "Populated room list", rooms = Just roomsList, allGames = Set.toList (Set.fromList (List.map .game_name roomsList))}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing userlist JSON"}, Cmd.none )

    GetUser json ->
      case Json.Decode.decodeValue User.decodeUser json of
        Ok user ->
          ( { model | debugString = "Updated user", user = user, colorInProgress = user.color}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing user JSON"}, Cmd.none )
          
    SetSorting key asc ->
      ( { model | sorting = (key, not asc) }
      , Cmd.none )
          
    Refresh ->
      ( { model | rooms = Nothing }
      , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "get_rooms"),
                        ( "content", Json.Encode.string "" ) ] ) ) )

    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    SetColor color ->
      ( { model | colorInProgress = color }, Cmd.none )
      
    SetRoomName str ->
      ( { model | roomNameInProgress = str }, Cmd.none )
      
    SetRoomMaxPlayers str ->
      ( { model | roomNumInProgress = str }, Cmd.none )
      
    SetRoomGameName str ->
      ( { model | roomGameInProgress = str }, Cmd.none )
      
    UpdateSettings ->
      let
        newColor = model.colorInProgress
        newName = if String.length model.nameInProgress > 0 then model.nameInProgress else model.user.nickname
        oldUser = model.user
        newUser = { oldUser | nickname = newName, color = newColor }
      in
      ( { model
       | nameInProgress = ""
       , toggleOptions = False
      }
      , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("action", Json.Encode.string "update_user"), ("content", User.encodeUser newUser) ] ) ) )

    ToggleModal ->
      ( { model | toggleModal = not model.toggleModal }, Cmd.none )

    ToggleOptions ->
      ( { model | toggleOptions = not model.toggleOptions }, Cmd.none )

    ToggleView v ->
      ( { model | toggleView = v }, Cmd.none )

    ToggleShowFullRooms ->
      ( { model | showFullRooms =  not model.showFullRooms }, Cmd.none )

    ToggleShowEmptyRooms ->
      ( { model | showEmptyRooms =  not model.showEmptyRooms }, Cmd.none )

    SetIgnoredGames ignoredGames ->
      ( { model | ignoredGames = ignoredGames }, Cmd.none )



decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "action" Json.Decode.string)
    (Json.Decode.field "content" Json.Decode.value)

countRooms : Maybe ( List ( Room ) ) -> Int
countRooms rooms =
  case rooms of
     Nothing -> 0
     Just rs -> List.length rs



countPlayers : Maybe ( List ( Room ) ) -> Int
countPlayers rooms =
  case rooms of
     Nothing -> 0
     Just rs -> List.foldl (\x y -> x.current_players + y) 0 rs

-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 30000 Ping
    , inputPort GetJSON
    ]


-- VIEW
drawRooms : Maybe ( List (Room) ) -> (String, Bool) -> Set ( String ) -> Bool -> Bool -> List ( Html Msg )
drawRooms mayberooms (key, asc) ignoredGames showFullRooms showEmptyRooms = 
  case mayberooms of
     Nothing -> [ div [class "server_row"] [ div [ class "message--loading_rooms"] [ text "Loading rooms..." ] ] ]
     Just rooms ->
      let
        filterRooms1 = 
          if showEmptyRooms then
            rooms
          else
            List.filter (\r -> r.current_players > 0) rooms

        filteredRooms =
          if showFullRooms then
            List.filter (\r -> not (Set.member r.game_name ignoredGames)) filterRooms1
          else
            List.filter (\r -> not (Set.member r.game_name ignoredGames) && (r.current_players < r.max_players)) filterRooms1
        
        sortedRooms =
          case key of
            "room_name" -> List.sortBy .room_name filteredRooms
            "game_name" -> List.sortBy .game_name filteredRooms
            "player_count" -> List.sortBy .current_players filteredRooms
            _ -> List.sortBy .room_name filteredRooms
      in
        case sortedRooms of
           [] -> [ div [class "server_row" ] [ div [ class "message--no_rooms"] [ text "There are no rooms to show!" ] ] ]
           rs ->
            if asc then
              List.map drawRoom rs
            else
              List.reverse (List.map drawRoom rs)

getURL : GameName -> String
getURL gn =
  case gn of
     Canoe -> "./canoe/?room="
     Codenames -> "./codenames/?room="
     Homeworlds -> "./homeworlds/?room="
     RicochetRobots -> "./robots/?room="
     Unknown -> "./lobby/?room="
     
getGameName : String -> GameName
getGameName s =
  case s of
     "Canoe" -> Canoe
     "Codenames" -> Codenames
     "Homeworlds" -> Homeworlds
     "Ricochet Robots" -> RicochetRobots
     _ -> Unknown

drawRoom : Room ->  Html Msg
drawRoom room =
  let
    gn = getGameName room.game_name
    url = getURL gn
  in
    div [class "server_row"]
    [ div [ class "col_players" ] [ text (String.fromInt room.current_players ++ "/" ++ String.fromInt room.max_players ) ]
    , div [ class "col_room_name" ] [ text room.room_name ]
    , div [ class "col_game_name" ] [ text room.game_name ]
    , div [ class "col_special" ] [  ]
    , div [ class "col_actions" ] [ a [ class "button", href (url ++ room.room_name) ] [ text "Join" ] ]
    ]

drawGameFilter : Set String -> String -> Html Msg
drawGameFilter ignoredGames game =
  if Set.member game ignoredGames then
    li [ class "inactive", onClick (SetIgnoredGames (Set.remove game ignoredGames))] [ text game ]
  else
    li [ class "", onClick (SetIgnoredGames (Set.insert game ignoredGames))] [ text game ]

drawEmptyGameFilter : Set String -> Html Msg
drawEmptyGameFilter ignoredGames =
  if Set.member "" ignoredGames then
    li [ class "inactive", onClick (SetIgnoredGames (Set.remove "" ignoredGames))] [ text "Show rooms without games" ]
  else
    li [ class "", onClick (SetIgnoredGames (Set.insert "" ignoredGames))] [ text "Show rooms without games" ]




view : Model -> Html Msg
view model =
  div [ class "container" ]
  [ div [ class "sidebar" ]
    [ div [ class "sidebar_box sidebar_box--user" ]
      [ h2 [] [ text "User", button [ class "icon toggle_options", onClick ToggleOptions ] [] ]
      , div [ class "username", style "color" model.user.color  ]
        [ span [ attribute "flow" "down", attribute "tooltip" ("UID: " ++ model.user.username), attribute "flow" "right" ] [ text model.user.nickname ]
        , span [ class "self", attribute "flow" "right", attribute "tooltip" "This is you!" ] [ span [ class "icon star" ] [] ]
        , if isMuted (Just model.user) then span [ class "self", attribute "flow" "right", attribute "tooltip" "Muted" ] [ span [ class "icon muted" ] [] ] else text ""
        , if isAdmin (Just model.user) then span [ class "self", attribute "flow" "right", attribute "tooltip" "Owner" ] [ span [ class "icon owner" ] [] ] else text ""
        ]
      , div [ class ("settings__flexbox" ++ if model.toggleOptions then "" else " hidden") ]
        [ div [ class "setting__input" ] [ input [ type_ "text", onInput SetName, placeholder "New name", value model.nameInProgress ] [] ]
        , div [ class "setting__input" ]
          [ select [ onInput SetColor ]
            [ option [ value "", style "color" "#707070" ] [ text "Change color" ]
            , option [ value "#e05e5e", style "color" "#e05e5e" ] [ text "red" ]
            , option [ value "#e09f5e", style "color" "#e09f5e" ] [ text "orange" ]
            , option [ value "#e0e05e", style "color" "#e0e05e" ] [ text "yellow" ]
            , option [ value "#9fe05e", style "color" "#9fe05e" ] [ text "lime" ]
            , option [ value "#5ee05e", style "color" "#5ee05e" ] [ text "dark sea" ]
            , option [ value "#5ee09f", style "color" "#5ee09f" ] [ text "aquamarine" ]
            , option [ value "#5ee0e0", style "color" "#5ee0e0" ] [ text "azure" ]
            , option [ value "#5e9fe0", style "color" "#5e9fe0" ] [ text "cornflower" ]
            , option [ value "#5e5ee0", style "color" "#5e5ee0" ] [ text "periwinkle" ]
            , option [ value "#9f5ee0", style "color" "#9f5ee0" ] [ text "dendrobium " ]
            , option [ value "#e05ee0", style "color" "#e05ee0" ] [ text "french rose" ]
            , option [ value "#e05e9f", style "color" "#e05e9f" ] [ text "barbie-mobile" ]
            , option [ value "#b19278", style "color" "#b19278" ] [ text "english elm" ]
            , option [ value "#e0e0e0", style "color" "#e0e0e0" ] [ text "gainsboro" ]
            ]
          ]
        , div [ class "setting__submit" ] [ input [ type_ "submit", class "submit button--big", value "Update", onClick UpdateSettings ] [] ]
          ]
            
      ]
    , div [ class "sidebar_box sidebar_box--stats" ]
      [ h2 [] [ text "Statistics" ]
      , div [] [ text (String.fromInt (countRooms model.rooms) ++ " rooms") ] 
      , div [] [ text (String.fromInt (countPlayers model.rooms) ++ " players") ] 
      ]
    , div [ class "sidebar_box sidebar_box--filter" ]
      [ h2 [] [ text "Filter Games" ]
      , ul []
        [ li [ class (if model.showFullRooms then "" else "inactive"), onClick ToggleShowFullRooms ] [ text "Show full rooms" ]
        , li [ class (if model.showEmptyRooms then "" else "inactive"), onClick ToggleShowEmptyRooms ] [ text "Show empty rooms" ]
        , drawEmptyGameFilter model.ignoredGames
        ]
      , ul []
        (List.map (drawGameFilter model.ignoredGames) model.allGames)
      ]
    , div [ class "sidebar_box sidebar_box--debug" ]
      [ h2 [] [ text "Debug" ]
      , text model.debugString ]
    ]
  , div [ class "main" ]
    [ div [ class "nav" ]
      [ div [ class "nav_space" ] [ ]
      , div [ class "nav_box" ] [ a [ class (if model.toggleView == "rooms" then "active" else ""), onClick (ToggleView "rooms") ] [ text "Rooms" ] ]
      , div [ class "nav_box" ] [ a [ class (if model.toggleView == "games" then "active" else ""), onClick (ToggleView "games") ] [ text "Games" ] ]
      , div [ class "nav_space" ] [ button [ onClick ToggleModal ] [ text "Create room" ] ]
      ]
    , div [ class "server_table" ]
      (List.concat
        [
          [ div [ class "server_row server_headers" ]
            [ div [ class "col_players" ] [ button [ id "player_count", onClick (SetSorting "player_count" (Tuple.second model.sorting))] [ text "Player count" ] ]
            , div [ class "col_room_name" ] [ h2 [ onClick (SetSorting "room_name" (Tuple.second model.sorting))] [ text "Room name" ] ]
            , div [ class "col_game_name" ] [ h2 [ onClick (SetSorting "game_name" (Tuple.second model.sorting))] [ text "Current game" ] ]
            , div [ class "col_special" ] [ h2 [] [ text "" ] ]
            , div [ class "col_actions" ] [ button [ id "refresh", onClick Refresh ] [ text "Refresh" ] ]
            ]
          ]
        , drawRooms model.rooms model.sorting model.ignoredGames model.showFullRooms model.showEmptyRooms
        ]
      )
    ]
  , div [ class ("lightbox" ++ if model.toggleModal then "" else " hidden") ]
    [ div [ class "modal" ]
      [ h1 [ ] [ text "Create room" ]
      , div [ class "modal_row" ] [ input [ type_ "text", placeholder "Room name", onInput SetRoomName ] [] ]
      , div [ class "modal_row" ]
        [ input [ type_ "text", placeholder "Max players", onInput SetRoomMaxPlayers ] []
        , select [ class "game_name", onInput SetRoomGameName ]
          [ option [ value "" ] [ text "No game (select one later)" ]
          , option [ value "Canoe"] [ text "Canoe" ]
          , option [ value "Codenames"] [ text "Codenames" ]
          , option [ value "Homeworlds"] [ text "Homeworlds" ]
          , option [ value "Ricochet Robots" ] [ text "Ricochet Robots" ]
          ]
        ]
      , div [ class "modal_row" ]
        [ button [ class "button--cancel button--big", onClick ToggleModal ] [ text "Cancel" ]
        , button [ class "button--create button--big", onClick CreateRoom ] [ text "Create room" ] ]
      ]
    ]
  ]
