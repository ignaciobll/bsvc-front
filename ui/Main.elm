module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Json.Decode as JD
import WebSocket
import Util exposing (hexStrToInt, toBinStr)
import Char exposing (fromCode)
import Task
-- Native Code
import FileReader exposing (NativeFile)
import FileReader.FileDrop as DZ


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

type alias FileManger =
    { filename : Maybe NativeFile
    , content : String
    , dragHovering : Int
    }

type alias Model =
    { registers : List Register
    , memory : Memory
    , file : FileManger
    }


init : (Model, Cmd Msg)
init =
    ( Model [] (Memory [] 0) (FileManger Nothing "" 0)
    , Cmd.none)


-- Update

type Msg
    = Registers (List Register)
    | MemoryUpdate MemoryDump
    | MemoryStart Int
    | WebSocketMessage String
    | Refresh String
    | LoadProgram
    | Step Int
    | Log String
    | OnDragEnter Int -- FileMsg
    | OnDrop (List NativeFile)
    | StartUpload
    | OnFileContent (Result FileReader.Error String)
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Refresh obj ->
            (model, WebSocket.send wsEndpoint obj)
        Registers lreg ->
            ({ model | registers = lreg }, Cmd.none)
        LoadProgram ->
            (model, WebSocket.send wsEndpoint "loadprogram")
        Step n ->
            (model, WebSocket.send wsEndpoint ("step " ++ (toString n)))
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

        OnDragEnter inc ->
            let
                oldFile = model.file
                newFile = { oldFile | dragHovering = oldFile.dragHovering + inc }
            in
            ( { model | file = newFile }, Cmd.none )

        OnDrop file ->
            case file of
                [ f ] ->
                    let
                        oldFile = model.file
                        newFile = { oldFile | filename = Just f, dragHovering = 0 }
                    in
                        ( { model | file = newFile }, getFileContents f )
                _ ->
                    let
                        oldFile = model.file
                        newFile = { oldFile | dragHovering = 0 }
                    in
                        ( { model | file = newFile }, Cmd.none )

        StartUpload ->
            (model, model.file.content |> sendFileToServer)

        OnFileContent res ->
            case res of
                Ok content ->
                    let
                        oldFile = model.file
                        newFile = { oldFile | content = content }
                    in
                        ( { model | file = newFile }, Cmd.none )
                Err err ->
                    Debug.crash (toString err)

        _ ->
            ( model, Cmd.none )

getFileContents : NativeFile -> Cmd Msg
getFileContents nf =
    FileReader.readAsTextFile nf.blob
        |> Task.attempt OnFileContent

sendFileToServer : String -> Cmd Msg
sendFileToServer raw =
    let
        content = "filecontent " ++ raw
    in
    WebSocket.send wsEndpoint content


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
        , viewProgram model
        ]
      
viewRegisters : List Register -> Html Msg
viewRegisters lr =
    table []
        ((tr [] [ th [] [ span [] [ text "Register" ] ]
                , th [] [ span [] [ text "Value"    ] ]
                , th [] [ span [] [ text "Decimal"  ] ]
                , th [] [ span [] [ text "Char"  ] ]
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
                                           0 -> 46 -- A dot char (.)
                                           _ -> c)
                    |> List.map (String.fromChar << fromCode)
                    |> String.join ""
    in
    tr []
       [ td [] [ span [ class "MemoryLine" ] [ text (toString n) ] ]
       , td [] [ span [ class "MemoryHex"  ] [ text hexMemory    ] ]
       , td [] [ span [ class "MemoryAscii"] [ text asciiMemory  ] ] 
       ]
        

viewProgram : Model -> Html Msg
viewProgram model =
    div []
        -- [ button [ onClick (LoadProgram) ] [ text ("LoadProgram") ]
        [ button [ onClick (Step 1) ] [ text ("Step") ]
        , viewDragArea model
        ]


viewDragArea : Model -> Html Msg
viewDragArea model =
    let
        dzAttrs_ =
            DZ.dzAttrs (OnDragEnter 1) (OnDragEnter -1) NoOp OnDrop

        dzClass =
            if model.file.dragHovering > 0 then
                class "drop-zone active" :: dzAttrs_
            else
                class "drop-zone" :: dzAttrs_
    in
        div [ class "panel" ] <|
            [ p [] [ text "Drag n Drop file below or use the file dialog to load file" ]
            , div dzClass
                [ input
                      [ type_ "file"
                      , FileReader.onFileChange OnDrop
                      , multiple False
                      ]
                      []
                ]
            , case model.file.filename of
                  Just nf ->
                      div []
                          [ span [] [ text nf.name ]
                          , button [ onClick StartUpload ] [ text "Upload" ]
                          , div [] [ code
                                         [ style [ ("white-space", "pre-wrap") ]
                                         ]
                                         [ viewCodeBlock model.file.content
                                         ]
                                   ]
                          ]
                  Nothing ->
                      text ""
            ]

viewCodeBlock : String -> Html Msg
viewCodeBlock cblock =
    let
        lineN = List.indexedMap (,) (String.lines cblock)
    in
        table []
            (List.map viewCodeLine lineN)


viewCodeLine : (Int, String) -> Html Msg
viewCodeLine (nline, line) =
    tr []
        [ td [] [ span [ class "codeLine"] [ text (toString nline)  ] ]
        , td [] [ span [ class "codeLine"] [ text line  ] ]
        ]

