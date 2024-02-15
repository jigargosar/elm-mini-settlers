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
    , nextOrderId : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        _ =
            applyN 100 updateOnTick model

        model : Model
        model =
            { drivers = initialDrivers
            , buildings = initialBuildings
            , step = 0
            , nextOrderId = 0
            }
    in
    ( model
    , Cmd.none
    )


type Msg
    = Tick


tickDurationInMillis =
    200


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every tickDurationInMillis (always Tick) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( updateOnTick model
            , Cmd.none
            )


updateOnTick : Model -> Model
updateOnTick =
    (\model -> { model | step = model.step + 1 })
        >> createAndAssignOrders
        >> stepDriversAndProcessEvents
        >> stepBuildings


createAndAssignOrders : Model -> Model
createAndAssignOrders =
    createOrders >> (\( orders, model ) -> List.foldl assignOrder model orders)


stepDriversAndProcessEvents : Model -> Model
stepDriversAndProcessEvents =
    let
        stepDrivers model =
            let
                ( drivers, events ) =
                    model.drivers
                        |> List.map driverStep
                        |> List.unzip
                        |> Tuple.mapSecond (List.filterMap identity)
            in
            ( events, { model | drivers = drivers } )

        processEvents ( events, model ) =
            List.foldl processDriverEvent model events
    in
    stepDrivers >> processEvents


stepBuildings : Model -> Model
stepBuildings model =
    { model | buildings = List.map (buildingStep model.step) model.buildings }


createOrders : Model -> ( Orders, Model )
createOrders model =
    let
        orderCtors : List (OrderId -> Order)
        orderCtors =
            model
                |> assembleRequests
                |> assembleResourceContenders
                |> List.concatMap createOrdersForResourceContenders

        ( nextOrderId, orders ) =
            orderCtors |> LE.mapAccuml (\nextId fn -> ( nextId + 1, fn nextId )) model.nextOrderId
    in
    ( orders, { model | nextOrderId = nextOrderId } )


createOrdersForResourceContenders : ResourceContenders -> List (OrderId -> Order)
createOrdersForResourceContenders { supplier, resource, available, contenders } =
    contenders |> LE.mapAccuml (createOrdersForContender supplier resource) available |> Tuple.second |> List.concat


createOrdersForContender : Building -> Resource -> Int -> Contender -> ( Int, List (OrderId -> Order) )
createOrdersForContender supplier resource available { consumer, required, route } =
    let
        toBeCreated =
            required |> atMost available
    in
    ( available - toBeCreated, List.repeat toBeCreated (createOrder { supplier = supplier, resource = resource, consumer = consumer, route = route }) )


createOrder { supplier, resource, consumer, route } orderId =
    let
        order : Order
        order =
            { id = orderId
            , resource = resource
            , initialDriverId = NE.head route |> .driver |> .id
            , from = from
            , to = to
            , steps = steps
            }

        steps =
            case route of
                ( _, [] ) ->
                    Pivot.fromCons { from = DestBuilding from, to = DestBuilding to } []

                ( firstRS, nextRS :: restRS ) ->
                    let
                        ( lastFromDriverRef, steps_ ) =
                            createInbetweenSteps firstRS nextRS restRS []
                    in
                    Pivot.fromCons { from = DestBuilding from, to = DestDriver <| createToHandoverRef firstRS nextRS }
                        (List.map (\s -> { from = DestDriver s.from, to = DestDriver s.to }) steps_
                            ++ [ { from = DestDriver lastFromDriverRef, to = DestBuilding to } ]
                        )

        ( from, to ) =
            ( supplier, consumer ) |> tmap buildingToRef
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
    , supplier : Building
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
        |> LE.gatherEqualsBy .supplier
        |> List.concatMap
            (NE.toList
                >> LE.gatherEqualsBy .resource
                >> List.map
                    (\( h, t ) ->
                        { supplier = h.supplier
                        , available = h.available
                        , resource = h.resource
                        , contenders =
                            (h :: t)
                                |> List.sortBy (.route >> NE.length)
                                |> List.map
                                    (\{ consumer, route, required } ->
                                        { consumer = consumer, required = required, route = route }
                                    )
                        }
                    )
            )


type alias Request =
    { supplier : Building
    , resource : Resource
    , available : Int
    , required : Int
    , route : Route
    , consumer : Building
    }


assembleRequests : Model -> List Request
assembleRequests model =
    model.buildings
        |> List.concatMap
            (\consumer ->
                consumer
                    |> buildingRequiredResources
                    |> List.filterMap
                        (\requirement ->
                            findShortestPathToSupplier requirement consumer model
                        )
            )


findShortestPathToSupplier : { required : Int, resource : Resource } -> Building -> Model -> Maybe Request
findShortestPathToSupplier { required, resource } consumer model =
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
        Search.Goal (Found { supplier, available, route }) _ ->
            Just
                { supplier = supplier
                , resource = resource
                , available = available
                , route = route
                , consumer = consumer
                , required = required
                }

        _ ->
            Nothing


type SearchState
    = Ongoing Route
    | Found { supplier : Building, available : Int, route : Route }


type alias Route =
    NE RouteSegment


type alias RouteSegment =
    { driver : Driver, dropGP : GP }


withGoalStatus state =
    ( state
    , case state of
        Ongoing _ ->
            False

        Found _ ->
            True
    )


initSearchState : Building -> Resource -> Buildings -> List RouteSegment -> RouteSegment -> ( SearchState, Bool )
initSearchState consumer resource buildings prevRouteSegments routeSegment =
    LE.findMap
        (\b ->
            if b.id /= consumer.id && driverCanServeGP b.entry routeSegment.driver then
                buildingAvailableOutResource resource b
                    |> Maybe.map
                        (\available ->
                            Found
                                { supplier = b
                                , available = available
                                , route = ( routeSegment, prevRouteSegments )
                                }
                        )

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

        Found _ ->
            []


searchStateRoute : SearchState -> Route
searchStateRoute state =
    case state of
        Ongoing route ->
            route

        Found { route } ->
            route


searchStateCost : SearchState -> Float
searchStateCost state =
    searchStateRoute state |> NE.length |> toFloat


assignOrder : Order -> Model -> Model
assignOrder order =
    updateDriverWithId order.initialDriverId (driverAssignOrder order)
        >> updateBuildingWithId order.from.id (buildingRegisterOutboundOrder order)
        >> updateBuildingWithId order.to.id (buildingRegisterInboundOrder order)


mapBuildings fn model =
    { model | buildings = fn model.buildings }


updateBuildingWithId id fn =
    mapBuildings (buildingsUpdateWithId id fn)


mapDrivers fn model =
    { model | drivers = fn model.drivers }


updateDriverWithId id fn =
    mapDrivers (driversUpdateWithId id fn)


processDriverEvent event =
    case event of
        DriverAtPickupDest o ->
            case orderPickupDest o of
                DestBuilding br ->
                    updateBuildingWithId br.id (buildingUpdateOnOrderPickup o.id)

                DestDriver dr ->
                    updateDriverWithId dr.id (driverNotifyHandoverCompleted o.id)

        DriverAtDropoffDest o ->
            case orderDropOffDest o of
                DestBuilding br ->
                    updateBuildingWithId br.id (buildingUpdateOnOrderDropoff o.id)

                DestDriver dr ->
                    updateDriverWithId dr.id (driverAssignOrder (orderUpdateNextStep o))


orderUpdateNextStep o =
    let
        steps =
            Pivot.goR o.steps
                |> ME.withDefaultLazy (\_ -> Debug.todo "unable to handoff no next step")

        updatedOrder =
            { o | steps = steps }
    in
    updatedOrder


updateExactlyOne pred fn list =
    let
        _ =
            if LE.count pred list == 1 then
                ()

            else
                let
                    _ =
                        Debug.log "debug" list
                in
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


colorApple =
    "#66b447"


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



-- ORDER


type alias Order =
    { id : OrderId
    , resource : Resource
    , initialDriverId : DriverId
    , from : BuildingRef
    , to : BuildingRef
    , steps : Pivot Step
    }


type alias OrderId =
    Int


type alias Orders =
    List Order


type alias Step =
    { from : Dest, to : Dest }


type Dest
    = DestBuilding BuildingRef
    | DestDriver DriverRef


orderCurrentStep : Order -> Step
orderCurrentStep o =
    Pivot.getC o.steps


orderCurrentPickupGP : Order -> GP
orderCurrentPickupGP o =
    o |> orderCurrentStep |> .from |> destGP


orderCurrentDropOffGP : Order -> GP
orderCurrentDropOffGP o =
    o |> orderCurrentStep |> .to |> destGP


orderShouldWaitForDropoffCompletion : Order -> Bool
orderShouldWaitForDropoffCompletion o =
    case o |> orderCurrentStep |> .to of
        DestDriver _ ->
            True

        DestBuilding _ ->
            False


orderPickupDest : Order -> Dest
orderPickupDest o =
    o |> orderCurrentStep |> .from


orderDropOffDest : Order -> Dest
orderDropOffDest o =
    o |> orderCurrentStep |> .to


destGP n =
    case n of
        DestBuilding { gp } ->
            gp

        DestDriver { gp } ->
            gp



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
    | PickingUp Order
    | DroppingOff Order
    | WaitingForHandover Order


initDriver : Int -> Road -> List Order -> Driver
initDriver id road pendingOrders =
    { id = id
    , pos = roadToNE road |> uncurry Pivot.fromCons
    , endpoints = roadEndpoints road
    , state = Idle
    , pendingOrders = pendingOrders
    }


driverCurrentGP : Driver -> GP
driverCurrentGP d =
    Pivot.getC d.pos


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
        |> ME.withDefaultLazy (\_ -> Debug.todo "handover for endpoint not found")


driverCanServeGP : GP -> Driver -> Bool
driverCanServeGP gp d =
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
    = DriverAtPickupDest Order
    | DriverAtDropoffDest Order


driverNotifyHandoverCompleted : Int -> Driver -> Driver
driverNotifyHandoverCompleted id d =
    case d.state of
        WaitingForHandover o ->
            if id == o.id then
                driverProcessPendingOrders d

            else
                Debug.todo "not WaitingForHandover"

        _ ->
            Debug.todo "not WaitingForHandover"


driverStep : Driver -> ( Driver, Maybe DriverEvent )
driverStep d =
    case d.state of
        Idle ->
            ( driverProcessPendingOrders d, Nothing )

        WaitingForHandover _ ->
            ( d, Nothing )

        PickingUp o ->
            driverMoveTowards (orderCurrentPickupGP o) d
                |> ME.withDefaultLazy
                    (\_ ->
                        ( { d | state = DroppingOff o }, Just (DriverAtPickupDest o) )
                    )

        DroppingOff o ->
            driverMoveTowards (orderCurrentDropOffGP o) d
                |> ME.withDefaultLazy
                    (\_ ->
                        ( if orderShouldWaitForDropoffCompletion o then
                            { d | state = WaitingForHandover o }

                          else
                            driverProcessPendingOrders d
                        , Just (DriverAtDropoffDest o)
                        )
                    )


driverProcessPendingOrders : Driver -> Driver
driverProcessPendingOrders d =
    case d.pendingOrders of
        o :: pendingOrders ->
            { d | state = PickingUp o, pendingOrders = pendingOrders }

        [] ->
            { d | state = Idle }


driverMoveTowards : GP -> Driver -> Maybe ( Driver, Maybe DriverEvent )
driverMoveTowards gp d =
    if driverCurrentGP d == gp then
        Nothing

    else
        Just ( driverMapPos (driverPosMoveTowards gp) d, Nothing )


driverMapPos fn d =
    { d | pos = fn d.pos }


driverPosMoveTowards gp pos =
    if Pivot.getR pos |> List.member gp then
        withRollback Pivot.goR pos

    else if Pivot.getL pos |> List.member gp then
        driverPosMoveTowards gp (Pivot.reverse pos)

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


defaultTransition =
    style "transition"
        ("translate $dur linear, scale $dur linear"
            |> String.replace "$dur" (ms tickDurationInMillis)
        )


ms n =
    String.fromFloat n ++ "ms"


viewVehicle gp state =
    group
        [ styleTranslate (gpToWorld gp)
        , defaultTransition
        , stroke strokeColor
        , strokeWidth strokeThickness
        ]
        [ square driverDiameter [ fill colorHQGray ]
        , let
            mbResource =
                case state of
                    Idle ->
                        Nothing

                    PickingUp _ ->
                        Nothing

                    DroppingOff o ->
                        Just o.resource

                    WaitingForHandover o ->
                        Just o.resource
          in
          circleWithDiameter driverDiameter
            [ maybeAttr mbResource (resourceColor >> fill)
            , defaultTransition
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
    List.filter (driverCanServeGP gp)


driversUpdateWithId : DriverId -> (Driver -> Driver) -> Drivers -> Drivers
driversUpdateWithId driverId fn drivers =
    updateExactlyOne (.id >> eq driverId) fn drivers



-- RESOURCE


type Resource
    = Water
    | Wood
    | Apple


resourceColor r =
    case r of
        Water ->
            colorWater

        Wood ->
            colorWood

        Apple ->
            colorApple



-- BUILDING


type alias Buildings =
    List Building


buildingsUpdateWithId : BuildingId -> (Building -> Building) -> Buildings -> Buildings
buildingsUpdateWithId buildingId fn =
    updateExactlyOne (.id >> eq buildingId) fn


type alias Building =
    { id : BuildingId
    , producer : Producer
    , stock : List SI
    , entry : GP
    , entryToCenterOffset : Float2
    , size : Int2
    , fill : String
    }


type alias BuildingId =
    Int


type Producer
    = Infinite { every : Int, inputs : List ResourceQuantity, output : Resource, elapsed : Int }
    | NoProduction


type alias ResourceQuantity =
    { resource : Resource, quantity : Int }


producerStep : Stock -> Producer -> Maybe ( Stock, Producer )
producerStep stock p =
    case p of
        NoProduction ->
            Nothing

        Infinite ({ every, inputs, output, elapsed } as r) ->
            stockPerformIO inputs output stock
                |> Maybe.map
                    (\newStock ->
                        if elapsed + 1 >= every then
                            ( newStock, Infinite { r | elapsed = 0 } )

                        else
                            ( stock, Infinite { r | elapsed = elapsed + 1 } )
                    )


type alias SI =
    { capacity : Int
    , io : IO
    , resource : Resource
    , available : Int
    , reserved : List OrderId
    }


type IO
    = In
    | Out


type alias Stock =
    List SI


siIsResource resource si =
    si.resource == resource


siIsOut si =
    si.io == Out


siIsIn si =
    si.io == In


siIsOutResource resource =
    allPass [ siIsResource resource, siIsOut ]


allPass preds val =
    List.map (\fn -> fn val) preds
        |> List.foldl (&&) True


siFreeCapacity si =
    si.capacity - (si.available + List.length si.reserved)


siHasFreeCapacity si =
    siFreeCapacity si > 0


siIsAvailable si =
    si.available > 0


siAvailableQuantity si =
    si.available


siConsume : Int -> SI -> Maybe SI
siConsume quantity si =
    if siAvailableQuantity si >= quantity then
        Just { si | available = si.available - quantity }

    else
        Nothing


siProduce : SI -> Maybe SI
siProduce si =
    if siFreeCapacity si > 0 then
        Just { si | available = si.available + 1 }

    else
        Nothing


stockPerformIO : List ResourceQuantity -> Resource -> Stock -> Maybe Stock
stockPerformIO inputs output stock =
    stock
        |> stockConsumeInputs inputs
        |> Maybe.andThen (stockStoreOutResource output)


stockConsumeInputs : List ResourceQuantity -> Stock -> Maybe Stock
stockConsumeInputs inputs stock =
    inputs
        |> List.foldl (\rq -> Maybe.andThen (stockConsumeInput rq)) (Just stock)


stockConsumeInput : ResourceQuantity -> Stock -> Maybe Stock
stockConsumeInput rq stock =
    let
        newStock =
            updateExactlyOne (allPass [ siIsIn, siIsResource rq.resource ])
                (\si -> si |> siConsume rq.quantity |> Maybe.withDefault si)
                stock
    in
    if stock == newStock then
        Nothing

    else
        Just newStock


stockStoreOutResource : Resource -> Stock -> Maybe Stock
stockStoreOutResource resource stock =
    let
        newStock =
            updateExactlyOne (siIsOutResource resource)
                (\si -> siProduce si |> Maybe.withDefault si)
                stock
    in
    if stock == newStock then
        Nothing

    else
        Just newStock


type alias BuildingRef =
    { id : Int, gp : GP }


buildingStep : Int -> Building -> Building
buildingStep _ b =
    case producerStep b.stock b.producer of
        Just ( stock, producer ) ->
            { b | stock = stock, producer = producer }

        Nothing ->
            b


buildingRequiredResources : Building -> List { required : Int, resource : Resource }
buildingRequiredResources b =
    b.stock
        |> List.filter siIsIn
        |> List.map
            (\si ->
                { required = siFreeCapacity si
                , resource = si.resource
                }
            )


buildingAvailableOutResource : Resource -> Building -> Maybe Int
buildingAvailableOutResource resource b =
    LE.find (siIsOutResource resource) b.stock |> Maybe.map .available


siIsInResourceWithFreeCapacity resource =
    allPass [ siIsIn, siIsResource resource, siHasFreeCapacity ]


siIsOutResourceAvailable resource =
    allPass [ siIsOut, siIsResource resource, siIsAvailable ]


siIsReserved orderId si =
    List.member orderId si.reserved


siIsOutReserved orderId =
    allPass [ siIsOut, siIsReserved orderId ]


siIsInReserved orderId =
    allPass [ siIsIn, siIsReserved orderId ]


buildingUpdateStockItem pred fn b =
    { b | stock = updateExactlyOne pred fn b.stock }


buildingRegisterInboundOrder : Order -> Building -> Building
buildingRegisterInboundOrder o =
    buildingUpdateStockItem
        (siIsInResourceWithFreeCapacity o.resource)
        (\si -> { si | reserved = o.id :: si.reserved })


buildingRegisterOutboundOrder : Order -> Building -> Building
buildingRegisterOutboundOrder o =
    buildingUpdateStockItem (siIsOutResourceAvailable o.resource)
        (\si -> { si | available = si.available - 1, reserved = o.id :: si.reserved })


buildingUpdateOnOrderPickup orderId =
    buildingUpdateStockItem (siIsOutReserved orderId)
        (\si -> { si | reserved = LE.remove orderId si.reserved })


buildingUpdateOnOrderDropoff : OrderId -> Building -> Building
buildingUpdateOnOrderDropoff orderId =
    buildingUpdateStockItem (siIsInReserved orderId)
        (\si -> { si | available = si.available + 1, reserved = LE.remove orderId si.reserved })


buildingToRef b =
    { id = b.id, gp = b.entry }


initialWellStock : Stock
initialWellStock =
    [ { io = Out, capacity = 4, available = 1, resource = Water, reserved = [] } ]


wellWaterProducer : Producer
wellWaterProducer =
    Infinite { every = 12, inputs = [], output = Water, elapsed = 0 }


b0 : Building
b0 =
    { id = 0
    , stock = initialWellStock
    , producer = wellWaterProducer
    , entry = ( 2, 3 )
    , entryToCenterOffset = ( 0, 1.5 )
    , size = ( 3, 2 )
    , fill = colorWater
    }


b1 : Building
b1 =
    { id = 1
    , stock = []
    , producer = NoProduction
    , entry = ( 5, 3 )
    , entryToCenterOffset = ( 0, 2 )
    , size = ( 3, 3 )
    , fill = colorHQGray
    }


b2 : Building
b2 =
    { id = 2
    , stock = initialWellStock
    , producer = wellWaterProducer
    , entry = ( 6, 3 )
    , entryToCenterOffset = ( 0, -1.5 )
    , size = ( 3, 2 )
    , fill = colorWater
    }


b3 : Building
b3 =
    let
        demand : SI
        demand =
            { io = In, capacity = 3, available = 0, reserved = [], resource = Water }

        supply : SI
        supply =
            { io = Out, capacity = 2, available = 0, reserved = [], resource = Apple }

        producer : Producer
        producer =
            Infinite { every = 3, inputs = [ { resource = Water, quantity = 1 } ], output = Apple, elapsed = 0 }
    in
    { id = 3

    -- , stock = initialWellStock
    -- , producer = wellWaterProducer
    , stock = [ demand, supply ]
    , producer = producer
    , entry = ( 7, 6 )
    , entryToCenterOffset = ( 1.5, 0 )
    , size = ( 2, 3 )
    , fill = colorApple
    }


b4 : Building
b4 =
    let
        demandWater : SI
        demandWater =
            { io = In, capacity = 3, available = 0, reserved = [], resource = Water }

        demandApple : SI
        demandApple =
            { io = In, capacity = 3, available = 0, reserved = [], resource = Apple }

        supply : SI
        supply =
            { io = Out, capacity = 2, available = 0, reserved = [], resource = Wood }

        producer : Producer
        producer =
            Infinite { every = 3, inputs = [ { resource = Water, quantity = 1 }, { resource = Apple, quantity = 1 } ], output = Wood, elapsed = 0 }
    in
    { id = 4
    , stock = [ demandWater, demandApple, supply ]
    , producer = producer
    , entry = ( 4, 8 )
    , entryToCenterOffset = ( 0, 1.5 )
    , size = ( 3, 2 )
    , fill = colorWood
    }


initialBuildings : Buildings
initialBuildings =
    [ b0, b1, b2, b3, b4 ]


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
            , viewStock b.stock
            ]
        , circleWithDiameter buildingEntryDiameter
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleTranslate entryCenter
            ]
        ]


viewStock : Stock -> Html msg
viewStock stock =
    let
        viewStockItem i si =
            let
                stored =
                    case si.io of
                        In ->
                            si.available

                        Out ->
                            si.available + List.length si.reserved

                string =
                    "$IO $RESOURCE $STORED"
                        |> String.replace "$IO" (Debug.toString si.io)
                        |> String.replace "$RESOURCE" (Debug.toString si.resource)
                        |> String.replace "$STORED" (Debug.toString stored)
            in
            group [ styleTranslate ( 0, toFloat i * cellGap ) ]
                [ words string [ fill strokeColor, style "font-size" "13px", style "font-family" "monospace" ] ]
    in
    group [ styleTranslate ( 0, -cellGap * 2 ) ] (List.indexedMap viewStockItem stock)



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


applyN n fn a =
    List.repeat n fn |> List.foldl (<|) a


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
