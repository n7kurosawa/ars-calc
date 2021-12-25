port module Main exposing(..)
import Browser
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Html
import String
import Dict exposing (Dict)

import Random

import Url exposing (percentEncode, percentDecode)
import Json.Encode as Encode
import Json.Decode as Decode

import Origami exposing (property, batch, noStyle, Style)
import Origami.Html exposing (..)
import Origami.Html.Events exposing (onClick, onCheck)
import Origami.Html.Attributes exposing (css, type_, checked, attribute)

import Numbers




main = Browser.element { init = init
                       , update = update
                       , view = view
                       , subscriptions = subscriptions
                       }

type alias Model =
  { onMenuWindow : Bool
  , shift : Shift
  , modeExp : Bool
  , modeHMS : HMS
  , modeRealRoot : Bool
  , decimalComma : Bool
  , stack : Stack
  , pending : String
  , mcell : MCell
  , seed : Random.Seed
  }

type Shift = ShiftNone | Shift2nd | Shift3rd | ShiftSTO

type HMS = HH | HMS | HMM

type alias Stack = List Numbers.Complex
type alias MCell = Dict Char Numbers.Complex

type alias Flags = { seed : Int, settings : String }

init : Flags -> (Model, Cmd.Cmd Msg)
init flags =
  let
    def = { modeRealRoot = False, modeExp = False, decimalComma = False }
    settingDecoder = Decode.map3 (\ x y z -> { modeRealRoot = x, decimalComma = y, modeExp = z } ) (Decode.field "modeRealRoot" Decode.bool) (Decode.field "decimalComma" Decode.bool) (Decode.field "modeExp" Decode.bool)
    settingValues = case Maybe.map (Decode.decodeString settingDecoder) <| percentDecode flags.settings of
      Just (Ok r) -> r
      _           -> def
      
    model =
      { onMenuWindow = False
      , shift = ShiftNone
      , modeExp = settingValues.modeExp
      , modeHMS = HH
      , modeRealRoot = settingValues.modeRealRoot
      , decimalComma = settingValues.decimalComma
      , stack = []
      , pending = ""
      , mcell = Dict.empty
      , seed = Random.initialSeed flags.seed
      }
  in let _ = Debug.log (Maybe.withDefault "Nothing" <| percentDecode flags.settings) ""
     in(model, Cmd.none)

type Msg
  = DoNothing
  | Toggle2nd
  | Toggle3rd
  | ToggleSTO
  | ToggleMode (Model -> Model)
  | Delete
  | Drop
  | Enter
  | InputChar String
  | ModifyStack (Stack -> Stack)
  | Const Numbers.Complex
  | CalcFun1 (Numbers.Complex -> Numbers.Complex)
  | CalcFun2 (Numbers.Complex -> Numbers.Complex -> Numbers.Complex)
  | RandomFloat
  | ModifyMCell (MCell -> MCell)
  | Store Char
  | Recall Char
  | ToggleWindow
  | SetModeRealRoot Bool
  | SetDecimalComma Bool

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update msg model =
  let
    newModel = updateModel msg model
  in
    (newModel, saveSettings newModel)
    -- (newModel, Cmd.none)

updateModel msg model =
  case msg of
    DoNothing -> model
    Toggle2nd -> doToggle2nd model
    Toggle3rd -> doToggle3rd model
    ToggleSTO -> doToggleSTO model
    ToggleMode f -> resetShift (Just << f) model
    Delete -> resetShift (Just << doDelete) model
    Drop -> resetShift (Just << doDrop) model
    Enter -> resetShift doEnter model
    InputChar s -> resetShift (doInputChar s) model
    ModifyStack f -> resetShift (doModifyStack f) model
    Const z -> resetShift (doConst z) model
    CalcFun1 f -> resetShift (doCalcFun1 f) model
    CalcFun2 f -> resetShift (doCalcFun2 f) model
    RandomFloat -> resetShift doRandomFloat model
    Store m -> resetShift (doStore m) model
    Recall m -> resetShift (doRecall m) model
    ModifyMCell f -> resetShift (doModifyMCell f) model
    ToggleWindow -> { model | onMenuWindow = not model.onMenuWindow }
    SetModeRealRoot p -> { model | modeRealRoot = p }
    SetDecimalComma p -> { model | decimalComma = p }

subscriptions : Model -> Sub.Sub Msg
subscriptions _ = Sub.none 

resetShift : (Model -> Maybe Model) -> Model -> Model
resetShift act model = case act model of
  Nothing       -> model
  Just newModel -> { newModel | shift = ShiftNone }

forceResetShift model = { model | shift = ShiftNone }

doToggle2nd model =
  if model.shift == Shift2nd then
    { model | shift = ShiftNone }
  else
    { model | shift = Shift2nd }

doToggle3rd model =
  if model.shift == Shift3rd then
    { model | shift = ShiftNone }
  else
    { model | shift = Shift3rd }

doToggleSTO model =
  if model.shift == ShiftSTO then
    { model | shift = ShiftNone }
  else
    { model | shift = ShiftSTO }

doToggleHMS model =
  case model.modeHMS of
    HH -> { model | modeHMS = HMS }
    HMS -> {model | modeHMS = HMM }
    HMM -> {model | modeHMS = HH}

doToggleFE model = { model | modeExp = not model.modeExp }

doDelete model =
  if String.isEmpty model.pending then
    model
  else
    { model | pending = String.dropRight 1 model.pending }

doDrop model =
  if not (String.isEmpty model.pending) then
    { model | pending = "" }
  else
    { model | stack = Maybe.withDefault [] (List.tail model.stack) }

doEnter model =
  if String.isEmpty model.pending then
    doDup model
  else
    commitPendingInput model

doDup model =
  case List.head model.stack of
    Just x  -> Just { model | stack = x :: model.stack }
    Nothing -> Just model

commitPendingInput model =
  if String.isEmpty model.pending then
    Just model
  else
    case Numbers.parseNumber (if model.decimalComma then "," else ".") model.pending of
      Just x -> Just { model | pending = "", stack = x :: model.stack }
      Nothing -> Nothing

doInputChar s model = Just { model | pending = String.append model.pending s }

doModifyStack f model = Maybe.map (\m -> { m | stack = f m.stack }) (commitPendingInput model)

doConst z model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> Just { newModel | stack = z :: newModel.stack }

doCalcFun1 f model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> case newModel.stack of
    x :: xs -> Just { newModel | stack = f x :: xs }
    _       -> Nothing

doCalcFun2 f model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> case newModel.stack of
    x :: y :: ys -> Just { newModel | stack = f y x :: ys }
    _            -> Nothing


doRandomFloat model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> case Random.step (Random.float 0 1) model.seed of
    (val, newSeed) -> Just { newModel | stack = { re = val, im = 0} :: newModel.stack, seed = newSeed }

doStore m model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> case newModel.stack of
    x :: xs -> Just { newModel | mcell = Dict.insert m x newModel.mcell, stack = xs }
    _       -> Nothing

doRecall m model = case commitPendingInput model of
  Nothing -> Nothing
  Just newModel -> case Dict.get m newModel.mcell of
    Just x -> Just { newModel | stack = x :: newModel.stack }
    Nothing -> Nothing

doModifyMCell f model = Just { model | mcell = f model.mcell }

view : Model -> Html.Html Msg
view model =
  if model.onMenuWindow then
    viewMenuWindow model
  else
    viewMainWindow model

viewMainWindow model =
  toHtml <|
    div [ css [ property "font-family" defaultFont
              , property "font-size" "3.8vw"
              , property "padding-left" "2vw"
              , property "padding-right" "2vw"
              ]
        ]
        [ statusView model
        , stackView model
        , upperButtonsView model
        , lowerButtonsView model
        ]

statusView model =
  div [ css [ property "display" "grid"
            , property "grid-template-columns" "4em 2em 4em 3em 10em"
            , property "padding-top" "1vh"
            , property "text-align" "left"
            , property "font-weight" "bold"
            ]
      ]
      [ div [ css []] [text (showShift model.shift)]
      , div [ css []] [text (showModeExp model.modeExp)]
      , div [ css []] [text (showModeHMS model.modeHMS)]
      , div [ css []] [text (showModeRealRoot model.modeRealRoot)]
      , div [ css []] [text ("stack: " ++ String.fromInt (List.length model.stack))]
      ]

stackView model =
  let
    formatBase = makeFormatType model
    format = case model.modeHMS of
        HMS -> Numbers.formatHMS formatBase Numbers.FormatHMS
        HMM -> Numbers.formatHMS formatBase Numbers.FormatHMM 
        HH -> Numbers.formatComplex formatBase
    stackX = Maybe.withDefault "" (List.head model.stack |> Maybe.map format)
    stackY = Maybe.withDefault "" (List.tail model.stack |> Maybe.andThen List.head |> Maybe.map format)
    styles = [ property "white-space" "nowrap", property "overflow-x" "auto", property "text-align" "right"]
  in
    div [ css [ property "background-color" "#bed2c3"
              , property "font-size" "6.4vw"
              , property "font-family" stackFont
              , property "letter-spacing" "0.05em"
              --, property "padding-top" ".1em"
              --, property "padding-bottom" ".1em"
              , property "display" "grid"
              , property "grid-template-rows" "1.6em 1.6em 1.6em"
              , property "grid-template-columns" "1auto 1fr"
              ]
        ]
        [ span [ css [ gridRow "1", gridColumn "1"] ] [ text "y:" ]
        , span [ css (styles ++ [ gridRow "1", gridColumn "2"]) ] [ text stackY ]
        , span [ css [ gridRow "2", gridColumn "1" ] ] [ text "x:" ]
        , span [ css (styles ++ [ gridRow "2", gridColumn "2"]) ] [ text stackX ]
        , span [ css [ gridRow "3", gridColumn "1" ] ] [text "\u{1405}"]  -- [text "\u{276F}\u{276F}"]
        , span [ css (styles ++ [ gridRow "3", gridColumn "2"]) ] [ text model.pending]
        ]

upperButtonsView model =
  div [ css [ property "display" "grid"
            , property "grid-template-columns" "1fr 1fr 1fr 1fr 1fr 1fr"
            , property "grid-template-rows" "10vh 10vh 10vh"
            , property "column-gap" ".1em"
            , property "row-gap" ".1em"
            , property "padding-top" ".5em"
            ]
      ]
      [ makeButton1a model Toggle2nd [span [css [property "color" color2nd, property "font-weight" "bold"]] [text "2nd"]]
      , makeButton1a model Toggle3rd [span [css [property "color" color3rd, property "font-weight" "bold"]] [text "3rd"]]
      , makeButton4 model ToggleSTO [text "STO"]  DoNothing [text ""] DoNothing [text ""] ToggleSTO
      --, makeButton3 model (ToggleMode doToggleFE) [text "F\u{21C4}E"] (InputChar ":") [text "HMS"] (ToggleMode doToggleHMS) [text "\u{21C4}H"]
      , makeButton3 model (ToggleMode doToggleFE) [text "F\u{21D4}E"] (InputChar ":") [text ":"] (ToggleMode doToggleHMS) [text "\u{21D4}HMS"]
       
      , makeButton2 model (CalcFun1 Numbers.degree2radian) [text "D→R"] (CalcFun1 Numbers.radian2degree) [text "R→D"]
      , makeButton1 model ToggleWindow [text "menu"]

      , makeButton3 model (InputChar "i") [text "i"] (CalcFun1 (r2c Numbers.real)) [text "Re"] (CalcFun1 (r2c Numbers.imag)) [text "Im"]
      , makeButton3 model (CalcFun1 Numbers.sin) [text "sin"] (CalcFun1 Numbers.asin) [text "asin"] (Const { re= Basics.pi, im = 0.0}) [text "π"]
      , makeButton2 model (CalcFun1 Numbers.cos) [text "cos"] (CalcFun1 Numbers.acos) [text "acos"]
      , makeButton2 model (CalcFun1 Numbers.tan) [text "tan"] (CalcFun1 Numbers.atan) [text "atan"]
      , makeButton2 model (CalcFun1 Numbers.inv) [text "1/x"] (CalcFun1 Numbers.log2) [text "lb"]
      -- , makeButton3 model (ModifyStack swapTop) [text "SWAP"] (ModifyStack rotateTop2Bot) [text "S\u{2b8c}"] (ModifyStack rotateBot2Top) [text "\u{2b8e}S"]
      , makeButton3 model (ModifyStack swapTop) [text "swap"] (ModifyStack rotateTop2Bot) [text "S↰"] (ModifyStack rotateBot2Top) [text "↳S"]

      , makeButton4 model (InputChar "e") [text "Exp"] (CalcFun2 Numbers.logBase) [text "log", sub [] [text "y"], text "x"] (Recall 'A') [text "A"] (Store 'A')
      , makeButton4 model (CalcFun2 Numbers.pow) [text "y", sup [] [text "x"]] (CalcFun2 (if model.modeRealRoot then Numbers.rootnReal else Numbers.rootn)) [sup [] [text "x"], text "√y"] (Recall 'B') [text "B"] (Store 'B')
      , makeButton4 model (CalcFun1 (if model.modeRealRoot then Numbers.sqrtReal else Numbers.sqrt)) [text "√x"] (CalcFun1 (if model.modeRealRoot then Numbers.cbrtReal else Numbers.cbrt)) [sup [] [text "3"], text "√x"] (Recall 'C') [text "C"] (Store 'C')
      , makeButton4 model (CalcFun1 Numbers.pow2) [text "x", sup [] [text "2"]] (CalcFun1 Numbers.pow3) [text "x", sup [] [text "3"]] (Recall 'D') [text "D"] (Store 'D')
      , makeButton4 model (CalcFun1 Numbers.log10) [text "lg"] (CalcFun1 Numbers.exp10) [text "10", sup [] [text "x"]] (Recall 'E') [text "E"] (Store 'E')
      , makeButton4 model (CalcFun1 Numbers.log) [text "ln"] (CalcFun1 Numbers.exp) [text "e", sup [] [text "x"]] (Recall 'F') [text "F"] (Store 'F')
      ]


lowerButtonsView model =
  div [ css [ property "display" "grid"
            , property "grid-template-columns" "1fr 1fr 1fr 1fr 1fr"
            , property "grid-template-rows" "10vh 10vh 10vh 10vh"
            , property "column-gap" ".1em"
            , property "row-gap" ".1em"
            , property "padding-top" ".4em"
            ] ]
    [ makeButton3 model (InputChar "7") [largetext "7"] (CalcFun1 Numbers.sinh) [text "sh"] (CalcFun1 Numbers.asinh) [text "ash"]
    , makeButton3 model (InputChar "8") [largetext "8"] (CalcFun1 Numbers.cosh) [text "ch"] (CalcFun1 Numbers.acosh) [text "ach"]
    , makeButton3 model (InputChar "9") [largetext "9"] (CalcFun1 Numbers.tanh) [text "th"] (CalcFun1 Numbers.atanh) [text "ath"]
    , makeButton1 model Delete [text "DEL"]
    , makeButton3 model Drop [text "DROP"] (ModifyStack stackClear) [text "SC"] (ModifyMCell mcellClear) [text "MC"]

    , makeButton3 model (InputChar "4") [largetext "4"] (CalcFun1 (r2c Numbers.abs)) [text "abs"] (CalcFun1 (r2c Numbers.arg)) [text "arg"]
    , makeButton1 model (InputChar "5") [largetext "5"]
    , makeButton1 model (InputChar "6") [largetext "6"]
    , makeButton1 model (CalcFun2 Numbers.mul) [largetext "×"]
    , makeButton1 model (CalcFun2 Numbers.div) [largetext "÷"]

    , makeButton1 model (InputChar "1") [largetext "1"]
    , makeButton1 model (InputChar "2") [largetext "2"]
    , makeButton1 model (InputChar "3") [largetext "3"]
    , makeButton1 model (CalcFun2 Numbers.add) [largetext "+"]
    , makeButton1 model (CalcFun2 Numbers.sub) [largetext "\u{2212}"]

    , makeButton2 model (InputChar "0") [largetext "0"] RandomFloat [text "RND"]
    , if model.decimalComma then
        makeButton1 model (InputChar ",") [largetext ","]
      else
        makeButton1 model (InputChar ".") [largetext "."]
    , makeButton3 model (InputChar "-") [largetext "(-)"] (CalcFun1 Numbers.neg) [text "\u{2212}x"] (CalcFun1 Numbers.conj) [text "x", sup [] [text "*"]]
    , makeButton4WithStyle [gridColumn "span 2"] model Enter [text "ENTER"] DoNothing [text ""] DoNothing [text ""] DoNothing
    ]


makeButton1 model m1 h1 = makeButton4 model m1 h1 DoNothing [text ""] DoNothing [text ""] DoNothing

makeButton1a model m1 h1 = makeButton4 model m1 h1 m1 [text ""] m1 [text ""] m1

makeButton2 model m1 h1 m2 h2 = makeButton4 model m1 h1 m2 h2 DoNothing [text ""] DoNothing

makeButton3 model m1 h1 m2 h2 m3 h3 = makeButton4 model m1 h1 m2 h2 m3 h3 DoNothing

makeButton4 = makeButton4WithStyle []

makeButton4WithStyle : List Style -> Model -> msg -> List (Html msg) -> msg -> List (Html msg) -> msg -> List (Html msg) -> msg -> Html msg
makeButton4WithStyle styles model m1 h1 m2 h2 m3 h3 m4 =
  let
    msg = case model.shift of
      ShiftNone -> m1
      Shift2nd -> m2
      Shift3rd -> m3
      ShiftSTO -> m4
  in
    button [ css ( [ property "display" "grid"
                   , property "grid-template-columns" "1fr 1fr"
                   , property "grid-template-rows" "1fr 1.5fr"
                   , property "padding-top" "1vh"
                   , property "background-color" "#ffffc0"
                   , property "border-width" "3pt"
                   , property "text-align" "center"
                   , property "font-family" defaultFont
                   , property "ontouchstart" "\"\""
                   , property "font-feature-settings" "palt"
                   ] ++ styles
                 )
           , onClick msg
           ]
           [ span [css [gridRow "1", gridColumn "1", property "color" color2nd, property "font-size" "3.6vw", property "font-weight" "bold", property "text-align" "left"] ] h2
           , span [css [gridRow "1", gridColumn "2", property "color" color3rd, property "font-size" "3.6vw", property "font-weight" "bold", property "text-align" "right"] ] h3
           , span [css [gridRow "2", gridColumn "1/3", property "font-size" "5.4vw", property "font-weight" "bold" ] ] h1
           ]

color2nd = "#ff4b00"
--color2nd = "#03af7a"
color3rd = "#005aff"

--defaultFont = "\"Saira Condensed\""
defaultFont = "\"Roboto Condensed\",\"Calibri\""
--stackFont = "\"Saira\""
stackFont = "Roboto, Calibri"

gridRow = property "grid-row"
gridColumn = property "grid-column"

showShift : Shift -> String
showShift s =
  case s of
    ShiftNone -> ""
    Shift2nd -> "2nd"
    Shift3rd -> "3rd"
    ShiftSTO -> "STO"

showModeExp : Bool -> String
showModeExp s =
  case s of
    False -> "F"
    True -> "E"


showModeHMS : HMS -> String
showModeHMS s =
  case s of
    HH -> ""
    HMS -> "HMS"
    HMM -> "HMM"

showModeRealRoot : Bool -> String
showModeRealRoot s =
  case s of
    True -> "√R"
    False -> "√C"

swapTop : Stack -> Stack
swapTop xx = case xx of
  (x :: y :: ys) -> y :: x :: ys
  _              -> xx

rotateTop2Bot : Stack -> Stack
rotateTop2Bot xx = case xx of
  (x :: xs) -> List.append xs [x]
  []        -> []

rotateBot2Top : Stack -> Stack
rotateBot2Top xx = List.reverse xx |> rotateTop2Bot |> List.reverse

stackClear : Stack -> Stack
stackClear x = []

mcellClear : MCell -> MCell
mcellClear x = Dict.empty

makeFormatType : Model -> Numbers.FormatType
makeFormatType model =  { formatExp = model.modeExp, decimalComma = model.decimalComma }

r2c : (Numbers.Complex -> Float) -> Numbers.Complex -> Numbers.Complex
r2c f = \z -> { re = f z, im = 0 }


largetext s = span [ css [property "font-size" "larger" ] ] [text s]

viewMenuWindow model =
  toHtml <|
    div [ css [ property "font-family" defaultFont
              , property "font-size" "5vw"
              , property "padding-left" "2vw"
              , property "padding-right" "2vw"
              ]
        ]
        [ div [ css [property "display" "flex", property "justify-content" "center",  property "padding-top" "1em" ]] [ button [ css [property "font-size" "6vw" ], onClick ToggleWindow ] [ text "Exit menu" ] ]
        , h2 [] [ text "Options" ]
        , ul []
          [ li []
            [ label []
              [ sup [] [text "n"]
              , text "√ accepts only real numbers. "
              , input [css [ property "transform" "scale(1.5)"], type_ "checkbox", checked model.modeRealRoot, onCheck SetModeRealRoot] []
              ]
            ]
          , li []
            [ text "Decimal separator "
            , label []
              [ input [type_ "radio", attribute "name" "decimalseparator", checked model.decimalComma, onCheck SetDecimalComma] []
              , text "comma"
              ]
            , label []
              [ input [type_ "radio", attribute "name" "decimalseparator", checked (not model.decimalComma), onCheck (not >> SetDecimalComma)] []
              , text "dot"
              ]
            ]
          ]
        , h2 [] [ text "About" ]
        , text "ARSCalc -- An RPN Scientific Calculator"
        ]






port saveSettingsToCookie : String -> Cmd msg

saveSettings model =
  saveSettingsToCookie <|
  percentEncode <|
  Encode.encode 0 <|
  Encode.object
    [ ("modeRealRoot", Encode.bool model.modeRealRoot)
    , ("modeExp", Encode.bool model.modeExp)
    , ("decimalCOmma", Encode.bool model.decimalComma)
    ]

