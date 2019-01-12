import Browser.Events as Mouse
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Browser
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import Browser.Events
import Task
import Browser.Dom
import Ports exposing (send, receive)

import Json.Encode as E
-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

-- Need a data structure that represents the current state of the board.
-- This will be in the form of a series of shapes or what have you being
-- applied to the board which is then folded into an svg.

type alias Drawing a = List (Element a)
type Element a = Line a a | Path (List a)
type alias Point = (Int, Int)
type alias Size = { w : Int, h : Int }
type alias Box = { x : Int, y : Int, w : Int, h : Int }

type alias Model =
  { mouseDown : Bool
  , nextElement : Maybe (Element Point)
  , drawing : Drawing Point
  , svgSize : Size
  , windowBox : Box
  , svgBox : Box
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model False Nothing [] (Size 0 0) (Box 0 0 0 0) (Box 0 0 0 0)
  ,
    let
      f s { viewport } = Init s (box viewport)
      box {x, y, width, height} = Box (round x) (round y) (round width) (round height)
    in
      Task.attempt (defaultError (Init (Size 100 100) (Box 0 0 1000 1000))) (Task.map2 f (Task.succeed (Size 500 300)) (Browser.Dom.getViewport))
  )

defaultError default result =
  case result of
    Ok x -> x
    Err _ -> default



-- UPDATE


type Msg
  = Start Int Int
  | Move Int Int
  | Stop Int Int
  -- | Resize Int Int
  | Init Size Box
  | DataFromJS (Result D.Error (Drawing Point))-- E.Value
  -- | SVGLoaded

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start x y ->
      ( { model | mouseDown = True
                , nextElement = Just (Path [mouseToBoardCoord (x,y) model, mouseToBoardCoord (x+5,y+5) model]) }
                -- , nextElement = let p = mouseToBoardCoord (x,y) model in Just (Line p p) }
      , Cmd.none
      )

    Move x y ->
      ( { model | nextElement = Maybe.map (updateElement (mouseToBoardCoord (x,y) model)) model.nextElement }, Cmd.none)

    Stop x y ->
      case model.nextElement of
        Nothing -> (model, Cmd.none)
        Just e -> ({ model | drawing = (updateElement (mouseToBoardCoord (x,y) model) e) :: model.drawing
                           , nextElement = Nothing
                           , mouseDown = False}
                  , send (jsonElement e))

    Init svg_ win ->
      let (h, w) = if win.w > win.h
                     then (win.h, (svg.w * win.h) // svg.h)
                     else ((svg.h * win.w) // svg.w, win.w)
          svg = Size 1600 900--win.w win.h
      in
      ( { model | windowBox = win, svgSize = svg, svgBox = Box 0 0 w h }, Cmd.none )
    DataFromJS (Ok drawing) -> ({ model | drawing = trace drawing }, Cmd.none) 
    DataFromJS (Err e) -> (trace2 e model, Cmd.none)

jsonElement : Element Point -> E.Value
jsonElement e =
  case e of
    Line p1 p2 -> E.object
      [ ("type", E.string "line")
      , ("start", point p1)
      , ("end", point p2)
      ]
    Path ps -> E.object [("type", E.string "path"), ("points", (E.list point ps))]

point : Point -> E.Value
point (x, y) = E.object [("x", E.int x), ("y", E.int y)]

mouseToBoardCoord : Point -> Model -> Point
mouseToBoardCoord (x, y) { svgBox, svgSize } = ((x * svgSize.w) // svgBox.w, (y * svgSize.h) // svgBox.h)

trace x = x -- Debug.log "" x
trace2 x y = y -- Debug.log (Debug.toString x) y
-- trace2 x y = Debug.log "" x 
-- trace x = x

updateElement : Point -> Element Point -> Element Point
updateElement pn e =
  case e of
    Line p1 _ -> Line p1 pn
    Path ps -> Path (pn::ps) --Path (ps ++ [pn])

-- VIEW

viewElement : Element Point -> Maybe (Svg.Svg msg)
viewElement e =
  case e of
    Line (x1, y1) (x2, y2) -> Just <| Svg.line [ SA.x1 (String.fromInt x1), SA.y1 (String.fromInt y1), SA.x2 (String.fromInt x2), SA.y2 (String.fromInt y2) ] []
    -- Path ps ->
    Path (p::ps) ->
      let
        show c (x, y) = c ++ (String.fromInt x) ++ " " ++ (String.fromInt y) ++ " "
        -- show (x, y) = (String.fromInt x) ++ ", " ++ (String.fromInt y) ++ " "
      in
        Just <| Svg.path [ SA.fill "none", SA.d (show "M" p ++ (List.foldr (\pn s -> show "L" pn ++ s) "" ps) ++ "")] []
        -- Just <| Svg.polyline [ SA.fill "none", SA.points (List.foldr (\pn s -> show pn ++ s) "" ps)] []
    _ -> Nothing


view : Model -> Html Msg
view model =

    Svg.svg
        [ SA.id "svg", SA.style ("stroke:rgb(0,0,0);stroke-width:10px;height:" ++ (String.fromInt (model.svgBox.h)) ++ "px;width:" ++ (String.fromInt (model.svgBox.w)) ++ "px;background-color:#EEE;top:0px; left0px; position:absolute;"), SA.version "1.1", SA.viewBox ("0 0 " ++ String.fromInt model.svgSize.w ++ " " ++ String.fromInt model.svgSize.h)
        -- [ SA.id "svg", SA.style ("stroke:rgb(0,0,0);stroke-width:10px;height:" ++ (String.fromInt (model.svgBox.h * 9 // 10)) ++ "px;width:" ++ (String.fromInt (model.svgBox.w * 9 // 10)) ++ "px;background-color:#EEE;display: block;margin:auto;"), SA.version "1.1", SA.viewBox ("0 0 " ++ String.fromInt model.svgSize.w ++ " " ++ String.fromInt model.svgSize.h)
        , SE.on "mousedown" (D.map2 Start pageX pageY)
        , SE.on "mouseup" (D.map2 Stop pageX pageY)]
        ((case (.nextElement model |> Maybe.andThen viewElement) of
          Just e -> [e]
          -- Just e -> []
          Nothing -> []) ++ (List.filterMap (viewElement) (.drawing model)))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ -- Mouse Movement
      if model.mouseDown
        then Mouse.onMouseMove (D.map2 Move pageX pageY)
        else Sub.none
    -- , -- Window Resize
    --   Browser.Events.onResize Resize
    ,
      receive (\x -> DataFromJS (D.decodeValue decodeData x))
    ]

decodePoint : D.Decoder Point
decodePoint = D.map2 (\x y -> (x, y)) (D.field "x" D.int) (D.field "y" D.int)

-- decodeLine : E.Value -> Decoder Element
-- decodeLine = 

decodeElement : D.Decoder (Element Point)
decodeElement = (D.field "type" D.string) |> D.andThen (\e -> 
  case e of
    "path" -> D.map Path (D.field "points" (D.list decodePoint))
    _ -> D.fail "error"
  )

decodeData : D.Decoder (Drawing Point)
decodeData = D.list decodeElement


pageX : D.Decoder Int
pageX =
  D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
  D.field "pageY" D.int
