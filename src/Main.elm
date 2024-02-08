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
import Search
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
    { drivers : Drivers
    , buildings : Buildings
    , step : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { drivers = initialDrivers
      , buildings = initialBuildings
      , step = 0
      }
      -- |> applyN 14 updateOnTick
    , Cmd.none
    )


applyN n fn a =
    if n <= 0 then
        a

    else
        applyN (n - 1) fn (fn a)


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
            ( updateOnTick model
            , Cmd.none
            )


updateOnTick model =
    model
        |> incStepCount
        |> createAndAssignOrders
        |> stepDrivers


incStepCount model =
    { model | step = model.step + 1 }


createAndAssignOrders model =
    createOrders model
        |> List.foldl assignOrder model


createOrders model =
    -- [ o04, o03, o01, o01, o10, o10 ]
    -- [ o01, o01, o10, o10 ]
    let
        foo : List (OrderId -> Order)
        foo =
            model
                |> assembleRequests
                |> assembleResourceContenders
                |> List.concatMap createOrdersForResourceContenders

        orders =
            foo |> List.indexedMap (\i fn -> fn (i + 10))
    in
    if model.step == 1 then
        -- [ o01 ]
        orders
            |> Debug.log "debug"

    else
        []


createOrdersForResourceContenders : ResourceContenders -> List (OrderId -> Order)
createOrdersForResourceContenders { producer, resource, available, contenders } =
    contenders |> LE.mapAccuml (createOrdersForContender producer resource) available |> Tuple.second |> List.concat


createOrdersForContender : Building -> Resource -> Int -> Contender -> ( Int, List (OrderId -> Order) )
createOrdersForContender producer resource available { consumer, required, route } =
    let
        toBeCreated =
            required |> atMost available
    in
    ( available - toBeCreated, List.repeat toBeCreated (createOrder { producer = producer, resource = resource, consumer = consumer, route = route }) )


createOrder { producer, resource, consumer, route } orderId =
    let
        order : Order
        order =
            { id = orderId
            , resource = resource
            , initialDriverId = NE.head route |> .driver |> .id
            , plan = plan
            -- , plan = o01.plan
            }

        ( from, to ) =
            ( producer, consumer ) |> tmap buildingToRef

        plan = 
            case route of
                ( _, [] ) ->
                    SingleStep from to

                ( firstRS, nextRS :: restRS ) ->
                    let
                        ( lastFromDriverRef, steps ) =
                            createInbetweenSteps firstRS nextRS restRS []
                    in
                    MultiStep B2D
                        { first = { from = from, to = createToHandoverRef firstRS nextRS }
                        , steps = steps
                        , last = { from = lastFromDriverRef, to = to }
                        }
    in
    order


createInbetweenSteps prev current rest_ acc =
    case rest_ of
        [] ->
            ( createFromHandoverRef prev current, List.reverse acc )

        next :: rest ->
            createInbetweenSteps current next rest (createStep prev current next :: acc)


createStep p c n =
    { from = createFromHandoverRef p c
    , to = createToHandoverRef c n
    }


createToHandoverRef : RouteSegment -> RouteSegment -> DriverRef
createToHandoverRef prev next =
    { id = next.driver.id, gp = driverHandoverGPForEndPoint prev.dropGP prev.driver }


createFromHandoverRef : RouteSegment -> RouteSegment -> DriverRef
createFromHandoverRef prev next =
    { id = prev.driver.id, gp = driverHandoverGPForEndPoint prev.dropGP next.driver }


type alias ResourceContenders =
    { resource : Resource
    , producer : Building
    , available : Int
    , contenders : List Contender
    }


type alias Contender =
    { consumer : Building
    , required : Int
    , route : Route
    }


assembleResourceContenders : List Request -> List ResourceContenders
assembleResourceContenders requests =
    requests
        |> LE.gatherEqualsBy .producer
        |> List.concatMap
            (NE.toList
                >> LE.gatherEqualsBy .stockItem
                >> List.map
                    (\( h, t ) ->
                        let
                            (StockItem available resource _) =
                                h.stockItem

                            producer =
                                h.producer
                        in
                        { producer = producer
                        , available = available
                        , resource = resource
                        , contenders =
                            (h :: t)
                                |> List.sortBy (.route >> NE.length)
                                |> List.map
                                    (\{ consumer, route, demand } ->
                                        let
                                            (Demand required _ _) =
                                                demand
                                        in
                                        { consumer = consumer, required = required, route = route }
                                    )
                        }
                    )
            )


type alias Request =
    { producer : Building
    , stockItem : StockItem
    , route : Route
    , consumer : Building
    , demand : Demand
    }


assembleRequests : Model -> List Request
assembleRequests model =
    model.buildings
        |> List.concatMap
            (\consumer ->
                consumer.demands
                    |> List.filterMap
                        (\demand ->
                            findShortestPathToProducer demand consumer model
                        )
            )


findShortestPathToProducer : Demand -> Building -> Model -> Maybe Request
findShortestPathToProducer ((Demand _ resource _) as demand) consumer model =
    let
        result =
            model.drivers
                |> driversFindAllServicingGP consumer.entry
                |> List.map (\d -> { driver = d, dropGP = consumer.entry })
                |> List.map (initSearchState consumer resource model.buildings [])
                |> Search.uniformCost { step = searchStateStep consumer resource model, cost = searchStateCost }
                |> Search.nextGoal
    in
    case result of
        Search.Goal (Found producer stockItem route) _ ->
            Just { producer = producer, stockItem = stockItem, route = route, consumer = consumer, demand = demand }

        _ ->
            Nothing


type SearchState
    = Ongoing Route
    | Found Building StockItem Route


type alias Route =
    NE RouteSegment


type alias RouteSegment =
    { driver : Driver, dropGP : GP }


withGoalStatus state =
    ( state
    , case state of
        Ongoing _ ->
            False

        Found _ _ _ ->
            True
    )


initSearchState : Building -> Resource -> Buildings -> List RouteSegment -> RouteSegment -> ( SearchState, Bool )
initSearchState consumer resource buildings prevRouteSegments routeSegment =
    LE.findMap
        (\b ->
            if b.id /= consumer.id && driverServicesGP b.entry routeSegment.driver then
                buildingStockItemForResource resource b |> Maybe.map (\stockItem -> Found b stockItem ( routeSegment, prevRouteSegments ))

            else
                Nothing
        )
        buildings
        |> Maybe.withDefault (Ongoing ( routeSegment, prevRouteSegments ))
        |> withGoalStatus


searchStateStep : Building -> Resource -> Model -> SearchState -> List ( SearchState, Bool )
searchStateStep consumer resource model state =
    case state of
        Ongoing (( routeSegment, restRouteSegments ) as route) ->
            model.drivers
                |> List.concatMap
                    (\d ->
                        driversConnectedGPs d routeSegment.driver
                            |> List.map (\gp -> { driver = d, dropGP = gp })
                            |> List.filter (\rs -> not (NE.member rs route))
                    )
                |> List.map (initSearchState consumer resource model.buildings (routeSegment :: restRouteSegments))

        Found _ _ _ ->
            []


searchStateRoute : SearchState -> Route
searchStateRoute state =
    case state of
        Ongoing route ->
            route

        Found _ _ route ->
            route


searchStateCost : SearchState -> Float
searchStateCost state =
    searchStateRoute state |> NE.length |> toFloat


assignOrder : Order -> Model -> Model
assignOrder order model =
    -- assign orders to buildings and drivers
    { model
        | drivers = driversAssignOrderTo order.initialDriverId order model.drivers
        , buildings = model.buildings |> buildingsReserveStockForOrder order
    }


stepDrivers model =
    let
        ( drivers, events ) =
            List.map stepDriver model.drivers
                |> List.unzip
                |> Tuple.mapSecond (List.filterMap identity)
    in
    { model | drivers = drivers } |> processDriverEvents events


processDriverEvents events model =
    let
        ( drivers, buildings ) =
            List.foldl processDriverEvent ( model.drivers, model.buildings ) events
    in
    { model | drivers = drivers, buildings = buildings }


processDriverEvent e ( drivers, buildings ) =
    case e of
        AtEndOf deliveryLeg o ->
            case ( deliveryLeg, o.plan ) of
                ( Dropoff, MultiStep B2D p ) ->
                    let
                        updatedOrder =
                            case p.steps of
                                [] ->
                                    { o | plan = MultiStep D2B p }

                                h :: t ->
                                    { o | plan = MultiStep (D2D ( h, t )) p }

                        nextDriverId =
                            p.first.to.id
                    in
                    ( drivers
                        |> driversAssignOrderTo nextDriverId updatedOrder
                    , buildings
                    )

                ( Dropoff, MultiStep (D2D ( step, pendingSteps )) p ) ->
                    let
                        updatedOrder =
                            case pendingSteps of
                                [] ->
                                    { o | plan = MultiStep D2B p }

                                h :: t ->
                                    { o | plan = MultiStep (D2D ( h, t )) p }

                        nextDriverId =
                            step.to.id
                    in
                    ( drivers
                        |> driversAssignOrderTo nextDriverId updatedOrder
                    , buildings
                    )

                ( Pickup, SingleStep from _ ) ->
                    ( drivers, buildings |> buildingsUpdateOnOrderPickup o )

                ( Pickup, MultiStep D2B p ) ->
                    let
                        prevDriverId =
                            p.last.from.id
                    in
                    ( drivers
                        |> driversNotifyHandoverCompletedTo prevDriverId o.id
                    , buildings
                    )

                ( Pickup, MultiStep (D2D ( step, _ )) p ) ->
                    let
                        prevDriverId =
                            step.from.id
                    in
                    ( drivers
                        |> driversNotifyHandoverCompletedTo prevDriverId o.id
                    , buildings
                    )

                _ ->
                    ( drivers, buildings )


updateExactlyOne pred fn list =
    let
        _ =
            if LE.count pred list == 1 then
                ()

            else
                Debug.todo "impl"
    in
    LE.updateIf pred fn list


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
        , div [ style "padding" "10px" ]
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

        -- , div [ style "padding" "10px" ] [ text "steps: ", text (String.fromInt model.step) ]
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


type alias OrderId =
    Int


type alias Order =
    { id : OrderId
    , resource : Resource
    , initialDriverId : DriverId
    , plan : Plan
    }


type Plan
    = SingleStep BuildingRef BuildingRef
    | MultiStep CurrentStep MultiStepPlan


initSingleStepPlan a b =
    SingleStep (buildingToRef a) (buildingToRef b)


type alias MultiStepPlan =
    { first : { from : BuildingRef, to : DriverRef }
    , steps : List { from : DriverRef, to : DriverRef }
    , last : { from : DriverRef, to : BuildingRef }
    }


type CurrentStep
    = B2D
    | D2D (NE { from : DriverRef, to : DriverRef })
    | D2B


type Node
    = BR BuildingRef
    | DR DriverRef


initBuildingNode =
    buildingToRef >> BR


nodeGP n =
    case n of
        BR { gp } ->
            gp

        DR { gp } ->
            gp


orderCurrentDestinationGPForDeliveryLeg : DeliveryLeg -> Order -> GP
orderCurrentDestinationGPForDeliveryLeg deliveryLeg o =
    orderCurrentNodeForDeliveryLeg deliveryLeg o |> nodeGP


orderCurrentNodeForDeliveryLeg : DeliveryLeg -> Order -> Node
orderCurrentNodeForDeliveryLeg deliveryLeg o =
    case deliveryLeg of
        Pickup ->
            orderCurrentPickupNode o

        Dropoff ->
            orderCurrentDeliveryNode o


orderPickupBuildingId : Order -> BuildingId
orderPickupBuildingId o =
    case o.plan of
        SingleStep { id } _ ->
            id

        MultiStep _ p ->
            p.first.from.id


orderCurrentPickupNode : Order -> Node
orderCurrentPickupNode o =
    case o.plan of
        SingleStep br _ ->
            BR br

        MultiStep B2D p ->
            BR p.first.from

        MultiStep D2B p ->
            DR p.last.from

        MultiStep (D2D ( h, _ )) p ->
            DR h.from


orderCurrentDeliveryNode : Order -> Node
orderCurrentDeliveryNode o =
    case o.plan of
        SingleStep _ br ->
            BR br

        MultiStep B2D p ->
            DR p.first.to

        MultiStep D2B p ->
            BR p.last.to

        MultiStep (D2D ( h, _ )) p ->
            DR h.to


initOrder : OrderId -> Building -> Building -> Resource -> Order
initOrder id from to resource =
    { id = id, initialDriverId = 0, resource = resource, plan = initSingleStepPlan from to }


o01 =
    initOrder 0 b0 b1 Water


o10 =
    initOrder 1 b1 b0 Wood


o03 : Order
o03 =
    { id = 2
    , resource = Water
    , initialDriverId = 0
    , plan =
        MultiStep B2D
            { first = { from = buildingToRef b0, to = { id = 1, gp = ( 6, 3 ) } }
            , steps = []
            , last = { from = { id = 0, gp = ( 7, 4 ) }, to = buildingToRef b3 }
            }
    }


o04 : Order
o04 =
    { id = 3
    , resource = Water
    , initialDriverId = 0
    , plan =
        MultiStep B2D
            { first = { from = buildingToRef b0, to = { id = 1, gp = ( 6, 3 ) } }
            , steps = [ { from = { id = 0, gp = ( 7, 4 ) }, to = { id = 2, gp = ( 7, 7 ) } } ]
            , last = { from = { id = 1, gp = ( 6, 8 ) }, to = buildingToRef b4 }
            }
    }



-- DRIVER


type alias DriverId =
    Int


type alias Driver =
    { id : DriverId
    , pos : Pivot GP
    , endpoints : ( GP, GP )
    , state : DriverState
    , pendingOrders : List Order
    }


type alias DriverRef =
    { id : Int, gp : GP }


type DriverState
    = Idle
    | InTransitFor DeliveryLeg Order
    | WaitingForHandover Order



-- Maybe Rename to TransitStage


type DeliveryLeg
    = Pickup
    | Dropoff


initDriver : Int -> Road -> List Order -> Driver
initDriver id road pendingOrders =
    { id = id
    , pos = roadToNE road |> uncurry Pivot.fromCons
    , endpoints = roadEndpoints road
    , state = Idle
    , pendingOrders = pendingOrders
    }


driversConnectedGPs : Driver -> Driver -> List GP
driversConnectedGPs a b =
    let
        ( e1, e2 ) =
            a.endpoints

        ( e3, e4 ) =
            b.endpoints
    in
    -- e1 == e3 || e1 == e4 || e2 == e3 || e2 == e4
    [ e1, e2 ] |> List.filter (\gp -> gp == e3 || gp == e4)


driverHandoverGPForEndPoint : GP -> Driver -> GP
driverHandoverGPForEndPoint ep d =
    let
        fn ls =
            case ls of
                f :: s :: _ ->
                    if f == ep then
                        Just s

                    else
                        Nothing

                _ ->
                    Nothing
    in
    Pivot.toList d.pos
        |> ME.oneOf [ fn, List.reverse >> fn ]
        |> ME.withDefaultLazy (\_ -> Debug.todo "handover for gp not found")


driverServicesGP : GP -> Driver -> Bool
driverServicesGP gp d =
    d.pos |> Pivot.toList |> List.member gp


driverAssignOrder : Order -> Driver -> Driver
driverAssignOrder o d =
    { d | pendingOrders = d.pendingOrders ++ [ o ] }


driverPathGPs : Driver -> List GP
driverPathGPs d =
    Pivot.toList d.pos


driverEndpoints : Driver -> ( GP, GP )
driverEndpoints d =
    d.endpoints


type DriverEvent
    = AtEndOf DeliveryLeg Order


driverNotifyHandoverCompleted : Int -> Driver -> Driver
driverNotifyHandoverCompleted id d =
    case d.state of
        WaitingForHandover o ->
            if id == o.id then
                processPendingOrders { d | state = Idle }

            else
                Debug.todo "not WaitingForHandover"

        _ ->
            Debug.todo "not WaitingForHandover"


stepDriver : Driver -> ( Driver, Maybe DriverEvent )
stepDriver d =
    case d.state of
        Idle ->
            ( processPendingOrders d, Nothing )

        WaitingForHandover _ ->
            ( d, Nothing )

        InTransitFor deliveryLeg o ->
            moveDriverTowardsEndOfDeliveryLeg deliveryLeg o d
                |> ME.withDefaultLazy
                    (\_ ->
                        ( case deliveryLeg of
                            Pickup ->
                                { d | state = InTransitFor Dropoff o }

                            Dropoff ->
                                case orderCurrentNodeForDeliveryLeg deliveryLeg o of
                                    BR _ ->
                                        processPendingOrders { d | state = Idle }

                                    DR _ ->
                                        { d | state = WaitingForHandover o }
                        , Just (AtEndOf deliveryLeg o)
                        )
                    )


processPendingOrders d =
    case d.pendingOrders of
        o :: pendingOrders ->
            { d | state = InTransitFor Pickup o, pendingOrders = pendingOrders }

        [] ->
            d


moveDriverTowardsEndOfDeliveryLeg deliveryLeg o d =
    let
        gp =
            orderCurrentDestinationGPForDeliveryLeg deliveryLeg o
    in
    if Pivot.getC d.pos == gp then
        Nothing

    else
        Just ( { d | pos = moveTowards gp d.pos }, Nothing )


moveTowards gp pos =
    if Pivot.getR pos |> List.member gp then
        withRollback Pivot.goR pos

    else if Pivot.getL pos |> List.member gp then
        moveTowards gp (Pivot.reverse pos)

    else
        Debug.todo ("invalid move pos." ++ Debug.toString ( gp, pos ))


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

                    InTransitFor Pickup _ ->
                        Nothing

                    InTransitFor Dropoff o ->
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



-- DRIVERS


type alias Drivers =
    List Driver


initialDrivers : Drivers
initialDrivers =
    [ initDriver 0 (initStraightRoad ( 2, 3 ) Right 6) []
    , initDriver 1 (initStraightRoad ( 7, 3 ) Down 6) []
    , initDriver 2 (initStraightRoad ( 7, 8 ) Left 6) []
    ]


driversFindAllServicingGP : GP -> Drivers -> Drivers
driversFindAllServicingGP gp =
    List.filter (driverServicesGP gp)


driversAssignOrderTo : DriverId -> Order -> Drivers -> Drivers
driversAssignOrderTo id order =
    updateDriverWithId id (driverAssignOrder order)


driversNotifyHandoverCompletedTo : DriverId -> OrderId -> Drivers -> Drivers
driversNotifyHandoverCompletedTo driverId orderId =
    updateDriverWithId driverId (driverNotifyHandoverCompleted orderId)


updateDriverWithId : DriverId -> (Driver -> Driver) -> Drivers -> Drivers
updateDriverWithId driverId fn drivers =
    updateExactlyOne (.id >> eq driverId) fn drivers



-- RESOURCE


type Resource
    = Water
    | Wood


resourceColor r =
    case r of
        Water ->
            colorWater

        Wood ->
            colorWood



-- BUILDING


type alias BuildingId =
    Int


type alias Building =
    { id : Int
    , resources : Int
    , demands : List Demand
    , stock : List StockItem
    , entry : GP
    , entryToCenterOffset : Float2
    , size : Int2
    , fill : String
    }


type Demand
    = Demand Int Resource (List OrderId)


type StockItem
    = StockItem Int Resource (List OrderId)


type alias BuildingRef =
    { id : Int, gp : GP }


buildingStockItemForResource : Resource -> Building -> Maybe StockItem
buildingStockItemForResource resource_ b =
    LE.find (\((StockItem _ resource _) as stockItem) -> resource == resource_) b.stock


buildingReserveStockForOrder : Order -> Building -> Building
buildingReserveStockForOrder o b =
    let
        stock =
            updateExactlyOne (\(StockItem unassigned resource _) -> resource == o.resource && unassigned > 0)
                (\(StockItem unassigned resource reservedOrderIds) ->
                    StockItem (unassigned - 1) resource (o.id :: reservedOrderIds)
                )
                b.stock
    in
    { b | stock = stock }


buildingUpdateOnOrderPickup orderId b =
    let
        stock =
            updateExactlyOne (\(StockItem _ _ reservedOrderIds) -> List.member orderId reservedOrderIds)
                (\(StockItem unassigned resource reservedOrderIds) ->
                    StockItem unassigned resource (LE.remove orderId reservedOrderIds)
                )
                b.stock
    in
    { b | stock = stock }


buildingToRef b =
    { id = b.id, gp = b.entry }


b0 =
    { id = 0, resources = 5, demands = [], stock = [ StockItem 5 Water [] ], entry = ( 2, 3 ), entryToCenterOffset = ( 0, 1.5 ), size = ( 3, 2 ), fill = colorWater }


b1 =
    { id = 1, resources = 5, demands = [], stock = [], entry = ( 5, 3 ), entryToCenterOffset = ( 0, 2 ), size = ( 3, 3 ), fill = colorHQGray }


b3 =
    { id = 3, resources = 5, demands = [], stock = [], entry = ( 7, 6 ), entryToCenterOffset = ( 1.5, 0 ), size = ( 2, 3 ), fill = colorWood }


b4 =
    { id = 4, resources = 5, demands = [ Demand 5 Water [] ], stock = [], entry = ( 4, 8 ), entryToCenterOffset = ( 0, 1.5 ), size = ( 3, 2 ), fill = colorWood }


initialBuildings =
    [ b0
    , b1
    , { id = 2, resources = 5, demands = [], stock = [], entry = ( 6, 3 ), entryToCenterOffset = ( 0, -1.5 ), size = ( 3, 2 ), fill = colorWater }
    , b3
    , b4
    ]


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

            -- , words (String.fromInt b.resources) [ fill strokeColor ]
            , viewStock b.stock
            , viewDemands b.demands
            ]
        , circleWithDiameter buildingEntryDiameter
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleTranslate entryCenter
            ]
        ]


viewStock stock =
    let
        viewStockItem (StockItem unassigned resource reservedOrderIds) =
            let
                ct =
                    unassigned + List.length reservedOrderIds
            in
            words ("P " ++ Debug.toString resource ++ ": " ++ String.fromInt ct) [ fill strokeColor ]
    in
    group [ styleTranslate ( 0, -cellGap * 2 ) ] (List.map viewStockItem stock)


viewDemands demands =
    let
        viewDemand (Demand unassigned resource reservedOrderIds) =
            let
                ct =
                    unassigned + List.length reservedOrderIds
            in
            words ("C " ++ Debug.toString resource ++ ": " ++ String.fromInt ct) [ fill strokeColor ]
    in
    group [ styleTranslate ( 0, cellGap * 2 ) ] (List.map viewDemand demands)



-- BUILDINGS


type alias Buildings =
    List Building


buildingsUpdateOnOrderPickup : Order -> Buildings -> Buildings
buildingsUpdateOnOrderPickup order =
    updateBuildingWithId (orderPickupBuildingId order) (buildingUpdateOnOrderPickup order.id)


buildingsReserveStockForOrder : Order -> Buildings -> Buildings
buildingsReserveStockForOrder order =
    updateBuildingWithId (orderPickupBuildingId order) (buildingReserveStockForOrder order)


updateBuildingWithId : BuildingId -> (Building -> Building) -> Buildings -> Buildings
updateBuildingWithId buildingId fn =
    updateExactlyOne (.id >> eq buildingId) fn



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


atLeast =
    max


atMost =
    min


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
