module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE exposing (onClick)
import List.Nonempty as NE
import Pivot exposing (Pivot)
import Svg exposing (Svg)
import Svg.Attributes as SA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


type alias GP =
    ( Int, Int )


allGPs =
    let
        ( w, h ) =
            ( 10, 10 )
    in
    times h (\y -> times w (\x -> ( x, y )))
        |> List.concat


type alias Float2 =
    ( Float, Float )


type alias Int2 =
    ( Int, Int )


view : Model -> Html Msg
view model =
    div []
        [ globalStyles
        , div [ style "padding" "20px" ]
            [ Svg.svg
                [ SA.viewBox "-25 -25 500 500"

                -- , style "border" "1px solid white"
                , style "width" "500px"
                , style "overflow" "visible"
                , stroke "none"
                , fill "none"
                ]
                ((allGPs |> List.map viewCellBorder)
                    ++ (buildings |> List.map viewBuilding)
                    ++ (drivers |> List.map viewDriver)
                )
            ]
        ]



-- Gameplay video
-- https://youtu.be/kINbjs4NffA?t=690


cellDiameter =
    50


cellRadius =
    cellDiameter / 2


cellSize =
    ( cellDiameter, cellDiameter )


gpToWorld gp =
    gp |> tmap toFloat |> mul2 cellSize


strokeColor =
    "#fff"


strokeThickness =
    2


cellGap =
    10


buildingEntryDiameter =
    -- cellDiameter - strokeThickness
    cellDiameter - cellGap


roadEndpointDiameter =
    cellDiameter - (cellGap * 2)


roadThickness =
    roadEndpointDiameter - cellGap


driverDiameter =
    roadEndpointDiameter


colorWood =
    let
        wood1 =
            "#814d2e"

        wood2 =
            "#a15c2a"
    in
    wood2


colorWater =
    "dodgerblue"


colorHQGray =
    "#666"


viewCellBorder gp =
    rect
        cellSize
        [ stroke "#666"
        , SA.strokeDasharray "6"
        , styleTranslate (gpToWorld gp)
        ]


type alias Driver =
    { id : Int
    , pos : Pivot GP
    , endpoints : ( GP, GP )
    }


drivers =
    [ ( ( 2, 3 ), ( 1, 0 ), 6 )
    ]
        |> List.indexedMap initDriver


initDriver : Int -> ( GP, GP, Int ) -> Driver
initDriver id ( endpoint1, dir_, len_ ) =
    let
        dir =
            tmap (clamp -1 1) dir_

        len =
            clamp 4 7 len_

        endpoint2 =
            tscale (len - 1) dir |> add2 endpoint1

        pathStart =
            add2 endpoint1 dir

        pathRest =
            times (len - 3) (\i -> tscale (i + 1) dir |> add2 pathStart)
    in
    { id = id
    , pos = Pivot.fromCons pathStart pathRest
    , endpoints = ( endpoint1, endpoint2 )
    }


viewDriver : Driver -> Html Msg
viewDriver d =
    let
        ( endpoint1, endpoint2 ) =
            d.endpoints

        roadPath =
            endpoint1 :: Pivot.toList d.pos ++ [ endpoint2 ]
    in
    group []
        [ viewRoad roadPath
        , viewRoadEndpoint endpoint1
        , viewRoadEndpoint endpoint2
        , viewDriver_ (Pivot.getC d.pos)
        ]


viewDriver_ gp =
    square driverDiameter [ styleTranslate (gpToWorld gp), stroke strokeColor, strokeWidth strokeThickness, fill colorHQGray ]


viewRoad : List GP -> Html Msg
viewRoad gps =
    let
        pts =
            gps |> List.map gpToWorld
    in
    group []
        [ polyline pts [ stroke strokeColor, strokeWidth (roadThickness + strokeThickness), SA.strokeLinecap "round" ]
        , polyline pts [ stroke "#ccc", strokeWidth (roadThickness - strokeThickness), SA.strokeLinecap "round" ]
        ]


viewRoadEndpoint gp =
    circleWithDiameter
        roadEndpointDiameter
        [ stroke strokeColor
        , fill "#333"
        , strokeWidth strokeThickness
        , styleTranslate (gpToWorld gp)
        ]


type alias Building =
    { entry : GP
    , entryToCenterOffset : Float2
    , size : Int2
    , fill : String
    }


buildings =
    [ { entry = ( 2, 3 ), entryToCenterOffset = ( 0, 1.5 ), size = ( 3, 2 ), fill = colorWater }
    , { entry = ( 5, 3 ), entryToCenterOffset = ( 0, 2 ), size = ( 3, 3 ), fill = colorHQGray }
    , { entry = ( 6, 3 ), entryToCenterOffset = ( 0, -1.5 ), size = ( 3, 2 ), fill = colorWater }
    , { entry = ( 7, 3 ), entryToCenterOffset = ( 1.5, 0 ), size = ( 2, 3 ), fill = colorWood }
    ]


viewBuilding b =
    let
        entryCenter =
            gpToWorld b.entry

        buildingCenter =
            gpToWorld b.entry |> add2 (mul2 cellSize b.entryToCenterOffset)

        buildingSize =
            mul2 b.size cellSize |> tmap (add -cellGap)
    in
    group []
        [ polyline [ entryCenter, buildingCenter ]
            [ stroke strokeColor
            , strokeWidth (cellDiameter / 2)
            ]
        , rect
            buildingSize
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleTranslate buildingCenter
            , SA.rx "5"
            ]
        , circleWithDiameter buildingEntryDiameter
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleTranslate entryCenter
            ]
        ]



-- SVG


strokeWidth n =
    SA.strokeWidth (String.fromFloat n)


styleTranslate ( x, y ) =
    style "translate" ([ px x, px y ] |> String.join " ")


styleScale n =
    style "scale" (String.fromFloat n)


px n =
    String.fromFloat n ++ "px"


stroke =
    SA.stroke


fill =
    SA.fill


group =
    Svg.g


circleWithDiameter d =
    circle (d * 0.5)


circle r attrs =
    Svg.circle
        (SA.r (String.fromFloat r)
            :: attrs
        )
        []


square l =
    rect ( l, l )


rect ( w, h ) attrs =
    Svg.rect
        (SA.width (String.fromFloat w)
            :: SA.x (String.fromFloat (-w / 2))
            :: SA.y (String.fromFloat (-h / 2))
            :: SA.height (String.fromFloat h)
            :: attrs
        )
        []


polyline pts attrs =
    let
        ptToString pt =
            pt |> tmap String.fromFloat |> tToList |> String.join " "

        ptsString =
            pts |> List.map ptToString |> String.join " "
    in
    Svg.polyline (SA.points ptsString :: attrs) []



-- BASICS


uncurry fn ( a, b ) =
    fn a b


pairTo b a =
    ( a, b )


type alias NE a =
    NE.ListNonempty a


times n fn =
    List.range 0 (n - 1) |> List.map fn


dec =
    add -1


add =
    (+)


sub =
    (-)


mul =
    (*)


tToList ( x, y ) =
    [ x, y ]


tscale n =
    tmap (mul n)


tmap fn =
    Tuple.mapBoth fn fn


add2 =
    tmap2 add


mul2 =
    tmap2 mul


tmap2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


globalStyles =
    Html.node "style" [] [ text """

:root{
    height:100%;
    font-family:Arial, sans-serif;
    font-size:20px;
    background:#111;
    color:#eee;
}

* { box-sizing:border-box; }

body{ margin:0; height:100%; }

    """ ]
