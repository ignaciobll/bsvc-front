import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Json.Decode as JD
import WebSocket
import Util exposing (hexStrToInt, toBinStr)
import Char exposing (fromCode)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

wsEndpoint = "ws://localhost:8765"
        
-- Model

type alias Register =
    { name : String
    , value : String }

type alias MemoryAddr = String
type alias MemoryDump = List (List MemoryAddr)
type alias Memory =
    { memoryDump : MemoryDump
    , memoryStart : Int
    }

type alias Model =
    { registers : List Register
    , memory : Memory
    }


init : (Model, Cmd Msg)
init =
    ( Model [] (Memory [] 0)
    , Cmd.none)


-- Update
        
type Msg
    = Registers (List Register)
    | MemoryUpdate MemoryDump
    | MemoryStart Int
    | WebSocketMessage String
    | Refresh String
    | Log String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Refresh obj ->
            (model, WebSocket.send wsEndpoint obj)
        Registers lreg ->
            ({ model | registers = lreg }, Cmd.none)
        MemoryStart pos ->
            case (model.memory.memoryStart + pos) < 0 of
                True ->
                    (model, Cmd.none)
                False ->
                    let
                        oldMemory = model.memory
                        oldMemStart = model.memory.memoryStart
                        query = "memory " ++ (toString (oldMemStart + pos))
                        newMemory = { oldMemory
                                        | memoryStart = oldMemStart + pos }
                    in
                        ( { model | memory = newMemory }
                        , WebSocket.send wsEndpoint query)
        MemoryUpdate lmem ->
            let
                oldMemory = model.memory
                newMemory = { oldMemory | memoryDump = lmem }
            in
                ({ model | memory = newMemory }, Cmd.none)
        WebSocketMessage content ->
            case parseContent content of
                Registers lreg ->
                    ({ model | registers = lreg }, Cmd.none)
                MemoryUpdate lmem ->
                    let
                        oldMemory = model.memory
                        newMemory = { oldMemory | memoryDump = lmem }
                    in
                        ({ model | memory = newMemory }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Log s ->
            (model, Cmd.none)

setMemoryDump : MemoryDump -> Memory -> Memory
setMemoryDump mdump mem =
    { mem | memoryDump = mdump }

setMemoryStart : Int -> Memory -> Memory
setMemoryStart n mem =
    { mem | memoryStart = n }

                
parseContent : String -> Msg
parseContent content =
    let
        parseReg = JD.decodeString (JD.list registerDecoder) content
        parseMem = JD.decodeString memoryDecoder content
    in
    case parseReg of
        Ok reg -> Registers reg
        Err x -> case parseMem of
                     Ok mem -> MemoryUpdate mem
                     Err x -> Log x

memoryDecoder : JD.Decoder MemoryDump
memoryDecoder =
    (JD.list (JD.list JD.string))

registerDecoder : JD.Decoder Register
registerDecoder =
    (JD.map2 Register (JD.field "name" JD.string) (JD.field "value" JD.string))


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen wsEndpoint WebSocketMessage 
                
            
-- View

view : Model -> Html Msg
view model =
    div []
        [ button [onClick (Refresh "registers")] [text "Refresh registers"]
        , viewRegisters model.registers
        , button [onClick (Refresh ("memory " ++ (toString model.memory.memoryStart))) ]
            [text "Refresh memory dump"]
        , viewMemory model.memory
        ]
      
viewRegisters : List Register -> Html Msg
viewRegisters lr =
    table []
        ((tr [] [ th [] [ span [] [ text "Register" ] ]
                , th [] [ span [] [ text "Value"    ] ]
                , th [] [ span [] [ text "Decimal"      ] ]
                ]
         ) :: (List.map viewRegister lr))
        
viewRegister : Register -> Html Msg
viewRegister r =
    tr []
        [ td [] [ span [class "RegisterName" ] [ text r.name ] ]
        , td [] [ span [class "RegisterValue"] [ text r.value ] ]
        , td [] [ span [class "RegisterDec"]   [ text ((toString << hexStrToInt) r.value)] ]
        , td [] [ span [class "RegisterAscii"] [ text ((toString << fromCode << hexStrToInt) r.value) ] ]
        ]        


viewMemory : Memory -> Html Msg
viewMemory memory =
    let
        lineN = List.indexedMap (\x memrow -> memory.memoryStart + x * 16) memory.memoryDump -- 16: length of memory row
    in
        div []
            [ div []
                  [ button [onClick (MemoryStart -16)] [ text "<<" ]
                  , span [class "MemoryStart" ] [text (toString memory.memoryStart) ]
                  , button [onClick (MemoryStart  16)] [ text ">>" ]
                  ]
            , table [] (List.map2 viewMemoryRow lineN memory.memoryDump) ]

viewMemoryRow : Int -> List MemoryAddr -> Html Msg
viewMemoryRow n lm =
    let
        hexMemory = String.join " " lm -- Future: Maybe more td, not a string line
        asciiMemory = List.map hexStrToInt lm
                    |> List.map (\c -> case c of
                                           0 -> 46  -- A dot char (.)
                                           _ -> c)
                    |> List.map (String.fromChar << fromCode)
                    |> String.join ""
    in
    tr []
       [ td [] [ span [ class "MemoryLine" ] [ text (toString n) ] ]
       , td [] [ span [ class "MemoryHex"  ] [ text hexMemory    ] ]
       , td [] [ span [ class "MemoryAscii"] [ text asciiMemory  ] ] 
       ]
        
