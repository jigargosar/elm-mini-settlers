module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, b, button, div, span, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE exposing (onClick)
import List.Extra as LE
import List.Nonempty as NE
import Maybe.Extra as ME
import Pivot exposing (Pivot)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { drivers : List Driver
    , buildings : List Building
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { drivers = initialDrivers
      , buildings = initialBuildings
      }
    , Cmd.none
    )


type Msg
    = Tick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 500 (always Tick) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                ( drivers, events ) =
                    List.map stepDriver model.drivers
                        |> List.unzip
                        |> Tuple.mapSecond (List.filterMap identity)

                ( drivers2, buildings ) =
                    List.foldl processDriverEvent ( drivers, model.buildings ) events
            in
            ( { model | drivers = drivers2, buildings = buildings }
            , Cmd.none
            )


processDriverEvent e ( drivers, buildings ) =
    case e of
        DeliveredOrder o ->
            case o.plan |> Pivot.getC |> .to of
                DR { id } ->
                    let
                        updatedPlan =
                            o.plan |> Pivot.goR |> ME.withDefaultLazy (\_ -> Debug.todo "impl")

                        updatedOrder =
                            { o | plan = updatedPlan }
                    in
                    ( drivers
                        |> updateExactlyOne (.id >> eq id) (\d -> { d | pendingOrders = d.pendingOrders ++ [ updatedOrder ] })
                    , buildings
                    )

                _ ->
                    ( drivers, buildings )

        _ ->
            ( drivers, buildings )


updateExactlyOne pred fn list =
    -- TODO: ensure one
    -- LE.updateIf pred fn list
    let
        _ =
            if LE.count pred list == 1 then
                ()

            else
                Debug.todo "impl"
    in
    LE.updateIf pred fn list



-- LE.count pred list
--     |> Just
--     |> ME.filter (eq 1)
--     |> Maybe.map
--         (\_ ->
--             LE.updateIf pred fn list
--         )
--     |> ME.withDefaultLazy (\_ -> Debug.todo "impl")


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
                    ++ (model.buildings |> List.map viewBuilding)
                    ++ (model.drivers |> List.map viewDriver)
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


type Dir
    = Up
    | Down
    | Left
    | Right


dirToOffset : Dir -> Int2
dirToOffset dir =
    case dir of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


type Road
    = Road (NE GP)


initStraightRoad : GP -> Dir -> Int -> Road
initStraightRoad from dir len_ =
    let
        len =
            clamp 4 7 len_

        pathRest =
            times len (\i -> tscale i (dirToOffset dir) |> add2 from)
                |> List.drop 1
    in
    Road ( from, pathRest )


roadEndpoints (Road ( head, tail )) =
    ( head, List.reverse tail |> List.head |> Maybe.withDefault head )


roadToNE (Road ne) =
    ne


type alias Step =
    { from : Node, to : Node }


initBBStep a b =
    { from = BR (buildingToRef a), to = BR (buildingToRef b) }


type Node
    = BR BuildingRef
    | DR DriverRef


nodeGP n =
    case n of
        BR { gp } ->
            gp

        DR { gp } ->
            gp


initBuildingNode =
    buildingToRef >> BR


type alias DriverRef =
    { id : Int, gp : GP }


type alias Order =
    { resource : Resource
    , plan : Pivot Step
    }


type alias Driver =
    { id : Int
    , pos : Pivot GP
    , endpoints : ( GP, GP )
    , state : DriverState
    , pendingOrders : List Order
    }


type DriverState
    = Idle
    | Pickingup Order
    | Delivering Order
    | WaitingForHandover Order


initialDrivers : List Driver
initialDrivers =
    [ initDriver 0
        (initStraightRoad ( 2, 3 ) Right 6)
        [ o03, o01, o01, o10, o10 ]
    , initDriver 1 (initStraightRoad ( 7, 3 ) Down 6) []
    ]


initDriver : Int -> Road -> List Order -> Driver
initDriver id road pendingOrders =
    { id = id
    , pos = roadToNE road |> uncurry Pivot.fromCons
    , endpoints = roadEndpoints road
    , state = Idle
    , pendingOrders = pendingOrders
    }


driverPathGPs : Driver -> List GP
driverPathGPs d =
    Pivot.toList d.pos


driverEndpoints : Driver -> ( GP, GP )
driverEndpoints d =
    d.endpoints


type DriverEvent
    = PickedupOrder Order
    | DeliveredOrder Order


orderCurrentPickupNode : Order -> Node
orderCurrentPickupNode o =
    o.plan |> Pivot.getC |> .from


orderCurrentDeliveryNode : Order -> Node
orderCurrentDeliveryNode o =
    o.plan |> Pivot.getC |> .to


stepDriver : Driver -> ( Driver, Maybe DriverEvent )
stepDriver d =
    case d.state of
        Idle ->
            ( processPendingOrders d, Nothing )

        Pickingup o ->
            let
                node =
                    orderCurrentPickupNode o

                gp =
                    nodeGP node
            in
            if Pivot.getC d.pos == gp then
                ( { d | state = Delivering o }, Just (PickedupOrder o) )

            else
                ( moveDriver gp d, Just (PickedupOrder o) )

        WaitingForHandover _ ->
            ( d, Nothing )

        Delivering o ->
            let
                node =
                    orderCurrentDeliveryNode o

                gp =
                    nodeGP node
            in
            if Pivot.getC d.pos == gp then
                case node of
                    BR _ ->
                        ( processPendingOrders { d | state = Idle }, Just (DeliveredOrder o) )

                    DR _ ->
                        ( { d | state = WaitingForHandover o }, Just (DeliveredOrder o) )

            else
                ( moveDriver gp d, Nothing )


processPendingOrders d =
    case d.pendingOrders of
        o :: pendingOrders ->
            { d | state = Pickingup o, pendingOrders = pendingOrders }

        [] ->
            d


moveDriver gp d =
    { d | pos = moveTowards gp d.pos }


moveTowards gp pos =
    if Pivot.getR pos |> List.member gp then
        withRollback Pivot.goR pos

    else if Pivot.getL pos |> List.member gp then
        moveTowards gp (Pivot.reverse pos)

    else
        Debug.todo "impl"


withRollback =
    Pivot.withRollback


viewDriver : Driver -> Html Msg
viewDriver d =
    let
        ( endpoint1, endpoint2 ) =
            driverEndpoints d

        roadPath =
            driverPathGPs d
    in
    group []
        [ viewRoad roadPath
        , viewRoadEndpoint endpoint1
        , viewRoadEndpoint endpoint2
        , viewVehicle (Pivot.getC d.pos) d.state
        ]


viewVehicle gp state =
    group
        [ styleTranslate (gpToWorld gp)
        , style "transition" "translate 500ms linear, scale 500ms linear"
        , stroke strokeColor
        , strokeWidth strokeThickness
        ]
        [ square driverDiameter [ fill colorHQGray ]
        , let
            mbResource =
                case state of
                    Idle ->
                        Nothing

                    Pickingup _ ->
                        Nothing

                    Delivering o ->
                        Just o.resource

                    WaitingForHandover o ->
                        Just o.resource
          in
          circleWithDiameter driverDiameter
            [ maybeAttr mbResource (resourceColor >> fill)
            , style "transition" "translate 500ms linear, scale 500ms linear"
            , style "scale" (ME.unwrap "0" (always "1") mbResource)
            ]
        ]


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


type Resource
    = Water
    | Wood


resourceColor r =
    case r of
        Water ->
            colorWater

        Wood ->
            colorWood


type alias Building =
    { id : Int
    , resources : Int
    , entry : GP
    , entryToCenterOffset : Float2
    , size : Int2
    , fill : String
    }


type alias BuildingRef =
    { id : Int, gp : GP }


buildingToRef b =
    { id = b.id, gp = b.entry }


b0 =
    { id = 0, resources = 5, entry = ( 2, 3 ), entryToCenterOffset = ( 0, 1.5 ), size = ( 3, 2 ), fill = colorWater }


b1 =
    { id = 1, resources = 5, entry = ( 5, 3 ), entryToCenterOffset = ( 0, 2 ), size = ( 3, 3 ), fill = colorHQGray }


b3 =
    { id = 3, resources = 5, entry = ( 7, 6 ), entryToCenterOffset = ( 1.5, 0 ), size = ( 2, 3 ), fill = colorWood }


initOrder from to resource =
    { resource = resource, plan = Pivot.singleton (initBBStep from to) }


o01 =
    initOrder b0 b1 Water


o10 =
    initOrder b1 b0 Wood


o03 =
    { resource = Water
    , plan =
        Pivot.fromCons
            { from = BR (buildingToRef b0), to = DR { id = 1, gp = ( 6, 3 ) } }
            [ { from = DR { id = 0, gp = ( 7, 4 ) }, to = BR (buildingToRef b3) } ]
    }


initialBuildings =
    [ b0
    , b1
    , { id = 2, resources = 5, entry = ( 6, 3 ), entryToCenterOffset = ( 0, -1.5 ), size = ( 3, 2 ), fill = colorWater }
    , b3
    ]


buildingIdEq id b =
    eq id b.id


viewBuilding : Building -> Svg msg
viewBuilding b =
    let
        entryCenter =
            gpToWorld b.entry

        buildingCenter =
            gpToWorld b.entry |> add2 (mul2 cellSize b.entryToCenterOffset)

        buildingSize =
            mul2 (tmap toFloat b.size) cellSize |> tmap (add -cellGap)
    in
    group []
        [ polyline [ entryCenter, buildingCenter ]
            [ stroke strokeColor
            , strokeWidth (cellDiameter / 2)
            ]
        , group [ styleTranslate buildingCenter ]
            [ rect
                buildingSize
                [ fill b.fill
                , stroke strokeColor
                , strokeWidth strokeThickness
                , SA.rx "5"
                ]
            , words (String.fromInt b.resources) [ fill strokeColor ]
            ]
        , circleWithDiameter buildingEntryDiameter
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleTranslate entryCenter
            ]
        ]



-- SVG


maybeAttr mb fn =
    Maybe.map fn mb |> Maybe.withDefault noAttr


noAttr =
    HA.style "" ""


words str attrs =
    Svg.text_ (SA.textAnchor "middle" :: SA.dominantBaseline "central" :: attrs) [ text str ]


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


eq =
    (==)


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
