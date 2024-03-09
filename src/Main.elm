module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, b, button, div, span, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import List.Extra as LE
import List.Nonempty as NE
import List.Nonempty.Extra as NEE
import Maybe.Extra as ME
import Pivot exposing (Pivot)
import Search
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


wellWaterProducer : Producer
wellWaterProducer =
    { every = 6
    , inputs = []
    , output = Water
    , elapsed = 0
    , inStock = []
    , outStock = [ initOutStockItem 4 Water ]
    }


wellConstructionStock : InStock
wellConstructionStock =
    [ initInStockItem 3 Wood ]


b0 : Building
b0 =
    { id = 0
    , state = Producing wellWaterProducer
    , entry = ( 2, 3 )
    , entryToCenterOffset = ( 0, 1.5 )
    , size = ( 3, 2 )
    , fill = colorWater
    }


b1 : Building
b1 =
    { id = 1
    , state = NoProduction
    , entry = ( 5, 3 )
    , entryToCenterOffset = ( 0, 2 )
    , size = ( 3, 3 )
    , fill = colorHQGray
    }


b2 : Building
b2 =
    { id = 2
    , state = UnderConstruction { inStock = wellConstructionStock, initialProducer = wellWaterProducer }
    , entry = ( 2, 8 )
    , entryToCenterOffset = ( 0, -1.5 )
    , size = ( 3, 2 )
    , fill = colorWater
    }


b3 : Building
b3 =
    let
        producer : Producer
        producer =
            { every = 3
            , inputs = [ { resource = Water, quantity = 1 } ]
            , output = Apple
            , elapsed = 0
            , inStock = [ initInStockItem 3 Water ]
            , outStock = [ initOutStockItem 2 Apple ]
            }
    in
    { id = 3
    , state = Producing producer
    , entry = ( 7, 6 )
    , entryToCenterOffset = ( 1.5, 0 )
    , size = ( 2, 3 )
    , fill = colorApple
    }


b4 : Building
b4 =
    let
        producer : Producer
        producer =
            { every = 3
            , inputs = [ { resource = Water, quantity = 1 }, { resource = Apple, quantity = 1 } ]
            , output = Wood
            , elapsed = 0
            , inStock = [ initInStockItem 3 Water, initInStockItem 3 Apple ]
            , outStock = [ initOutStockItem 2 Wood ]
            }
    in
    { id = 4
    , state = Producing producer
    , entry = ( 4, 8 )
    , entryToCenterOffset = ( 0, 1.5 )
    , size = ( 3, 2 )
    , fill = colorWood
    }


initialBuildings : Buildings
initialBuildings =
    [ b0, b1, b2, b3, b4 ]


type alias Model =
    { drivers : Drivers
    , nextDriverId : DriverId
    , buildings : Buildings
    , nextBuildingId : BuildingId
    , roadPosts : RoadPosts
    , step : Int
    , nextOrderId : OrderId
    , pointer : Float2
    , tool : Tool
    , camera : Camera
    }


type Tool
    = PlaceBlueprint Blueprint
    | CreateRoad (List GP)
    | Destroy
    | None


type alias Blueprint =
    Building


type alias Buildings =
    List Building


init : () -> ( Model, Cmd Msg )
init () =
    let
        _ =
            applyN 500 updateOnTick model

        model : Model
        model =
            { drivers = initialDrivers
            , nextDriverId = 200
            , buildings = initialBuildings
            , nextBuildingId = 100
            , roadPosts = initRoadPostsFromDrivers initialDrivers
            , step = 0
            , nextOrderId = 0
            , pointer = tscale 0.5 canvasSize |> add2 canvasPageOffset
            , tool =
                b3
                    |> blueprintRotate
                    |> blueprintRotate
                    |> blueprintRotate
                    |> blueprintRotate
                    -- |> Debug.log "debug"
                    |> PlaceBlueprint
                    |> always (CreateRoad [])
                    |> always Destroy
            , camera = initialCamera
            }
    in
    ( model
        -- |> applyN 8 updateOnTick
        -- |> mockDestroyBuilding
        |> applyN 11 updateOnTick
        |> mockDestroyDriver
    , Cmd.none
    )


mockDestroyDriver : Model -> Model
mockDestroyDriver model =
    case findById 1 model.drivers of
        Nothing ->
            model

        Just d ->
            destroyDriver d model


mockDestroyBuilding : Model -> Model
mockDestroyBuilding model =
    case findById 3 model.buildings of
        Nothing ->
            model

        Just b ->
            destroyBuilding b model


updateById x list =
    if LE.count (idEq x.id) list == 1 then
        Just (LE.setIf (idEq x.id) x list)

    else
        Nothing


findById id list =
    LE.find (idEq id) list


idEq id =
    .id >> eq id


updateExactlyOneWithId id fn =
    updateExactlyOne (.id >> eq id) fn


updateExactlyOneWithResource resource fn =
    updateExactlyOne (.resource >> eq resource) fn


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


type Msg
    = Tick
    | PointerMoved Float2
    | PointerTapped Float2
    | KeyDown String


tickDurationInMillis =
    100


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every tickDurationInMillis (always Tick)
        , BE.onMouseMove (JD.map PointerMoved pageXYDecoder)
        , BE.onKeyDown (JD.map KeyDown (JD.field "key" JD.string))
        , BE.onMouseDown (JD.map PointerTapped pageXYDecoder)
        ]


pageXYDecoder : Decoder Float2
pageXYDecoder =
    JD.map2 Tuple.pair (JD.field "pageX" JD.float) (JD.field "pageY" JD.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( updateOnTick model, Cmd.none )

        PointerMoved pointer ->
            ( { model | pointer = pointer }
                |> updateOnPointerMoved
            , Cmd.none
            )

        PointerTapped pointer ->
            updateOnPointerTap { model | pointer = pointer }

        KeyDown key ->
            case key of
                " " ->
                    updateOnPointerTap model

                "`" ->
                    ( { model | tool = None }, Cmd.none )

                "Escape" ->
                    ( { model | tool = None }, Cmd.none )

                "z" ->
                    ( { model | tool = Destroy }, Cmd.none )

                "1" ->
                    ( { model | tool = CreateRoad [] }, Cmd.none )

                "2" ->
                    ( { model | tool = PlaceBlueprint b2 }, Cmd.none )

                "3" ->
                    ( { model | tool = PlaceBlueprint b3 }, Cmd.none )

                "4" ->
                    ( { model | tool = PlaceBlueprint b4 }, Cmd.none )

                "r" ->
                    case model.tool of
                        PlaceBlueprint blueprint ->
                            ( { model | tool = blueprint |> blueprintRotate |> PlaceBlueprint }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


withoutCmd model =
    ( model, Cmd.none )


updateOnPointerTap model =
    case model.tool of
        PlaceBlueprint blueprint ->
            ( model |> addNewBuildingFromBlueprint blueprint, Cmd.none )

        CreateRoad gps ->
            updateCreateRoadToolOnPointerTap gps model
                |> withoutCmd

        Destroy ->
            ( destroyAtPointer model, Cmd.none )

        None ->
            ( model, Cmd.none )


destroyAtPointer : Model -> Model
destroyAtPointer model =
    let
        gp =
            pointerToGP model.camera model.pointer
    in
    model
        |> destroyDrivers (driversFindAllServicingGP gp model.drivers)
        |> destroyBuildings (List.filter (buildingOccupiesGP gp) model.buildings)


destroyDrivers drivers model =
    List.foldl destroyDriver model drivers


destroyDriver d model =
    let
        ( removedOrders, drivers ) =
            model.drivers
                |> driversUpdateOnDriverDestroyed d.id
                |> Tuple.mapSecond (reject (idEq d.id))

        roadPostIsAttachedToAnyDriver rp =
            drivers |> List.any (driverEndpoints >> tmember rp.id)
    in
    { model
        | drivers = drivers
        , buildings =
            model.buildings
                |> List.map (buildingRemoveOrderReservationsIfAny removedOrders)
        , roadPosts =
            model.roadPosts
                |> List.map (roadPostRemoveOrderReservationsIfAny removedOrders)
                |> List.filter roadPostIsAttachedToAnyDriver
    }


tmember x ( a, b ) =
    x == a || x == b


destroyBuildings : Buildings -> Model -> Model
destroyBuildings buildings model =
    List.foldl destroyBuilding model buildings


destroyBuilding : Building -> Model -> Model
destroyBuilding b model =
    let
        ( removedOrders, drivers ) =
            model.drivers
                |> driversUpdateOnBuildingDestroyed b.id
    in
    { model
        | drivers = drivers
        , buildings =
            reject (idEq b.id) model.buildings
                |> List.map (buildingRemoveOrderReservationsIfAny removedOrders)
        , roadPosts =
            model.roadPosts
                |> List.map (roadPostRemoveOrderReservationsIfAny removedOrders)
    }


updateCreateRoadToolOnPointerTap : List GP -> Model -> Model
updateCreateRoadToolOnPointerTap gps model =
    let
        pointerGP =
            pointerToGP model.camera model.pointer
    in
    case gps of
        [] ->
            { model | tool = CreateRoad [ pointerGP ] }

        h :: t ->
            Just ( h, t )
                -- |> ME.filter (NE.head >> eq pointerGP)
                |> Maybe.andThen initRoad
                |> Maybe.map
                    (\road ->
                        let
                            driver =
                                initDriver model.nextDriverId road []
                        in
                        { model
                            | nextDriverId = model.nextDriverId + 1
                            , drivers = driver :: model.drivers
                            , tool = CreateRoad [ pointerGP ]
                            , roadPosts =
                                List.foldl
                                    addNewRoadPostIfAbsent
                                    model.roadPosts
                                    (roadEndpoints road |> tToList)
                        }
                    )
                |> Maybe.withDefault model


updateOnPointerMoved : Model -> Model
updateOnPointerMoved model =
    case model.tool of
        CreateRoad gps ->
            let
                gp =
                    pointerToGP model.camera model.pointer
            in
            case gps of
                [] ->
                    model

                h :: [] ->
                    if areAdjcent gp h then
                        { model | tool = CreateRoad [ gp, h ] }

                    else
                        model

                h :: h2 :: t ->
                    if gp == h2 then
                        { model | tool = CreateRoad (h2 :: t) }

                    else if areAdjcent gp h && not (List.member gp gps) && List.length gps < 7 then
                        { model | tool = CreateRoad (gp :: gps) }

                    else
                        model

        _ ->
            model


areAdjcent : GP -> GP -> Bool
areAdjcent a b =
    List.member (subBy2 a b |> tmap abs) [ ( 0, 1 ), ( 1, 0 ) ]


addNewBuildingFromBlueprint : Blueprint -> Model -> Model
addNewBuildingFromBlueprint blueprint model =
    let
        building =
            buildingFromBlueprintAt (pointerToWorld model.camera model.pointer) blueprint
    in
    { model
        | buildings = { building | id = model.nextBuildingId } :: model.buildings
        , nextBuildingId = 1 + model.nextBuildingId
    }


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


createOrder : { supplier : Building, resource : Resource, consumer : Building, route : Route } -> OrderId -> Order
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

        steps : Pivot Step
        steps =
            case route of
                ( { driver }, [] ) ->
                    Pivot.fromCons { driverId = driver.id, from = DestBuilding from, to = DestBuilding to } []

                ( firstRS, nextRS :: restRS ) ->
                    let
                        ( ( lastDriverId, lastFromDest ), inbetweenSteps ) =
                            createInbetweenSteps firstRS nextRS restRS []
                    in
                    Pivot.fromCons { driverId = firstRS.driver.id, from = DestBuilding from, to = createToDest firstRS nextRS }
                        (inbetweenSteps
                            ++ [ { driverId = lastDriverId, from = lastFromDest, to = DestBuilding to } ]
                        )

        ( from, to ) =
            ( supplier, consumer ) |> tmap buildingToRef
    in
    order


createInbetweenSteps :
    RouteSegment
    -> RouteSegment
    -> List RouteSegment
    -> List Step
    -> ( ( DriverId, Dest ), List Step )
createInbetweenSteps prev current initialRest acc =
    case initialRest of
        [] ->
            ( ( current.driver.id, createFromDest prev current ), List.reverse acc )

        next :: newRest ->
            createInbetweenSteps current next newRest (createStep prev current next :: acc)


createStep : RouteSegment -> RouteSegment -> RouteSegment -> Step
createStep p c n =
    { driverId = c.driver.id
    , from = createFromDest p c
    , to = createToDest c n
    }


createToDest : RouteSegment -> RouteSegment -> Dest
createToDest prev next =
    DestRoadPost { roadPostId = prev.dropGP, gp = driverHandoverGPForEndPoint prev.dropGP prev.driver }


createFromDest : RouteSegment -> RouteSegment -> Dest
createFromDest prev next =
    DestRoadPost { roadPostId = prev.dropGP, gp = driverHandoverGPForEndPoint prev.dropGP next.driver }


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
    mapBuildings (updateExactlyOneWithId id fn)


mapDrivers fn model =
    { model | drivers = fn model.drivers }


updateDriverWithId id fn =
    mapDrivers (updateExactlyOneWithId id fn)


mapRoadPosts fn model =
    { model | roadPosts = fn model.roadPosts }


updateRoadPostWithId id fn =
    mapRoadPosts (updateExactlyOneWithId id fn)


findRoadPostById : RoadPostId -> Model -> RoadPost
findRoadPostById roadPostId model =
    findById roadPostId model.roadPosts
        |> ME.withDefaultLazy (\_ -> Debug.todo "roadPostId not found")


setRoadPost : RoadPost -> Model -> Model
setRoadPost roadPost model =
    { model
        | roadPosts =
            updateById roadPost model.roadPosts
                |> ME.withDefaultLazy (\_ -> Debug.todo "roadPost not found")
    }


processDriverEvent : DriverEvent -> Model -> Model
processDriverEvent event =
    case event of
        DriverAtPickupDest o ->
            case orderPickupDest o of
                DestBuilding br ->
                    updateBuildingWithId br.id (buildingUpdateOnOrderPickup o.resource o.id)

                DestRoadPost r ->
                    updateRoadPostWithId r.roadPostId (roadPostNotifyOrderPickedup o.id)

        DriverAtDropoffDest o ->
            case orderDropOffDest o of
                DestBuilding br ->
                    updateBuildingWithId br.id (buildingUpdateOnOrderDropoff o.resource o.id)

                DestRoadPost r ->
                    let
                        updatedOrder =
                            orderUpdateNextStep o
                    in
                    withRollback
                        (tryOrderHandOffToRoadPost r.roadPostId updatedOrder
                            >> Maybe.map
                                (notifyDriverOfHandoffCompletion o >> handOffOrderToDriver updatedOrder)
                        )


tryOrderHandOffToRoadPost : RoadPostId -> Order -> Model -> Maybe Model
tryOrderHandOffToRoadPost roadPostId order model =
    findRoadPostById roadPostId model
        |> roadPostTryAssignOrder order
        |> Maybe.map (\roadPost -> setRoadPost roadPost model)


handOffOrderToDriver : Order -> Model -> Model
handOffOrderToDriver order =
    updateDriverWithId (orderCurrentlyAssignedDriverId order) (driverAssignOrder order)


notifyDriverOfHandoffCompletion : Order -> Model -> Model
notifyDriverOfHandoffCompletion order =
    updateDriverWithId (orderCurrentlyAssignedDriverId order) (driverNotifyHandoverCompleted order.id)


orderUpdateNextStep o =
    { o
        | steps =
            Pivot.goR o.steps
                |> ME.withDefaultLazy (\_ -> Debug.todo "unable to handoff no next step")
    }


type alias GP =
    ( Int, Int )


allGPs =
    let
        ( w, h ) =
            ( 20, 20 )
    in
    times h (\y -> times w (\x -> ( x, y )))
        |> List.concat


gridGPs =
    let
        list =
            List.range -20 20
    in
    list
        |> List.map (\y -> list |> List.map (\x -> ( x, y )))
        |> List.concat


type alias Float2 =
    ( Float, Float )


type alias Int2 =
    ( Int, Int )


view : Model -> Html Msg
view model =
    div []
        [ globalStyles
        , div [ style "display" "flex", style "flex-direction" "column", style "padding" "10px", style "gap" "10px" ]
            [ viewSvg model

            -- , div [ style "display" "flex", style "flex-direction" "column" ]
            --     [ div [] [ text "steps: ", text (String.fromInt model.step) ]
            --     , div [] [ text "pointer: ", text (Debug.toString model.pointer) ]
            --     ]
            ]
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    div []
        [ Svg.svg
            [ viewBoxFromSizeWithOriginAtCenter canvasSize
            , style "display" "block"
            , style "outline" "1px solid dodgerblue"
            , style "width" (px canvasWidth)
            , style "height" (px canvasHeight)
            , style "overflow" "visible"
            , stroke "none"
            , fill "none"

            -- , HE.onMouseUp PointerTapped
            , style "user-select" "none"
            ]
            [ group [ styleTranslate cameraPan, styleScale cameraZoom ]
                (viewWorldContent model)
            , rect canvasSize [ stroke "white", strokeWidth 1, style "stroke-dasharray" (spaced [ 50 ]) ]
            ]
        ]


viewWorldContent : Model -> List (Svg Msg)
viewWorldContent model =
    [ group [] []
    , group [] (gridGPs |> List.map viewCellBorder)
    , group [] (model.buildings |> List.map viewBuilding)
    , keyedGroup [] (model.drivers |> List.map viewKeyedDriver)
    , group [] (model.roadPosts |> List.map viewRoadPost)
    , viewTool (pointerToWorld model.camera model.pointer) model.tool

    -- , viewPointerIndicator (pointerToWorld model.camera model.pointer)
    -- , group [] (gridGPs |> List.map viewCellBorder)
    , circle 10 [ fill "#fff" ]
    ]


keyedGroup =
    Svg.Keyed.node "g"


viewTool point tool =
    case tool of
        PlaceBlueprint blueprint ->
            viewBlueprint point blueprint

        CreateRoad gps ->
            viewCreateRoadTool point gps

        Destroy ->
            viewDestroyTool point

        None ->
            group [] []


viewDestroyTool point =
    group [ styleMoveToGP (worldToGP point) ]
        [ words "X" [ style "scale" "3", fill "red" ] ]


viewCreateRoadTool point gps =
    case gps of
        [] ->
            group [ style "opacity" "0.8" ] [ viewRoadPost (initRoadPost (point |> worldToGP)) ]

        h :: t ->
            let
                roadPostGP =
                    LE.last t |> Maybe.withDefault h
            in
            group []
                [ viewRoad gps
                , viewRoadPost (initRoadPost roadPostGP)
                ]


type alias Camera =
    { pan : Float2
    , zoom : Float
    }


initialCamera : Camera
initialCamera =
    let
        zoom =
            1.3

        pan =
            cellSize
                |> mul2 ( -5, -6 )
                |> tscale zoom
    in
    { pan = pan
    , zoom = zoom
    }


panCamera unitDir pan =
    pan
        |> subBy2 (tscale cellDiameter unitDir)


cameraPan =
    cellSize
        |> mul2 ( -5, -6 )
        |> tscale cameraZoom


cameraZoom =
    1.3


canvasSize =
    ( 950, 600 )


canvasWidth =
    Tuple.first canvasSize


canvasHeight =
    Tuple.second canvasSize


viewBoxFromSizeWithOriginAtCenter ( w, h ) =
    let
        ( l, t ) =
            ( w, h ) |> tscale -0.5
    in
    viewBoxFromLTWH l t w h


viewBoxFromLTWH l t w h =
    [ l, t, w, h ]
        |> spaced
        |> SA.viewBox


spaced nums =
    nums |> List.map String.fromFloat |> String.join " "


viewBlueprint point b =
    group [ style "opacity" "0.8" ] [ viewBuilding (buildingFromBlueprintAt point b) ]


viewPointerIndicator point =
    square cellDiameter
        [ fill colorWood
        , style "opacity" "0.5"
        , style "opacity" "0.8"
        , styleMoveToGP (point |> worldToGP)
        ]


canvasPageOffset =
    ( 10, 10 )


pointerToGP camera pointer =
    pointerToWorld camera pointer |> worldToGP


pointerToWorld camera pointer =
    pointer
        |> subBy2 (sum2 [ canvasPageOffset, tscale 0.5 canvasSize, camera.pan ])
        |> tscale (1 / camera.zoom)



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


worldToGP ( x, y ) =
    ( x / cellDiameter, y / cellDiameter ) |> tmap round


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


styleMoveToGP gp =
    styleTranslate (gpToWorld gp)


viewCellBorder gp =
    group [ styleMoveToGP gp ]
        [ rect
            cellSize
            [ stroke "#666"
            , SA.strokeDasharray "6"
            ]
        , words (Debug.toString gp) [ fill "#333", style "font-size" "13", style "font-family" "monospace" ]
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


initRoad : NE GP -> Maybe Road
initRoad ne =
    let
        len =
            NE.length ne
    in
    if
        -- valid size
        (clamp 4 7 len == len)
            -- no overlap
            && (NEE.unique ne == ne)
            -- contineous
            && (List.map2 areAdjcent (NE.tail ne) (NE.rest ne) |> List.all identity)
    then
        Just (Road ne)

    else
        Nothing


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


roadEndpoints (Road ne) =
    ( NE.head ne, NE.last ne )


roadToNE (Road ne) =
    ne



-- ROAD POST


type alias RoadPost =
    { id : GP
    , stockItems : List RoadPostStockItem
    }


type alias RoadPostStockItem =
    { resource : Resource
    , maybeOrderId : Maybe OrderId
    }


type alias RoadPostId =
    GP


type alias RoadPosts =
    List RoadPost


initRoadPost : GP -> RoadPost
initRoadPost gp =
    { id = gp, stockItems = [] }


initRoadPostsFromDrivers : Drivers -> RoadPosts
initRoadPostsFromDrivers drivers =
    drivers
        |> List.concatMap (driverEndpoints >> tToList)
        |> LE.unique
        |> List.map initRoadPost


addNewRoadPostIfAbsent : GP -> RoadPosts -> RoadPosts
addNewRoadPostIfAbsent gp roadPosts =
    if List.any (idEq gp) roadPosts then
        roadPosts

    else
        initRoadPost gp :: roadPosts


roadPostNotifyOrderPickedup : OrderId -> RoadPost -> RoadPost
roadPostNotifyOrderPickedup orderId rp =
    rp.stockItems
        |> LE.find (.maybeOrderId >> eq (Just orderId))
        |> Maybe.map
            (\stockItem ->
                { rp | stockItems = LE.remove stockItem rp.stockItems }
            )
        |> ME.withDefaultLazy (\_ -> Debug.todo "roadPost order not found for pickup")


roadPostRemoveOrderReservationsIfAny : Orders -> RoadPost -> RoadPost
roadPostRemoveOrderReservationsIfAny orders rp =
    List.foldl roadPostRemoveOrderReservationIfAny rp orders


roadPostRemoveOrderReservationIfAny : Order -> RoadPost -> RoadPost
roadPostRemoveOrderReservationIfAny o rp =
    { rp | stockItems = reject (.maybeOrderId >> eq (Just o.id)) rp.stockItems }


roadPostTryAssignOrder : Order -> RoadPost -> Maybe RoadPost
roadPostTryAssignOrder o rp =
    if List.length rp.stockItems < 3 then
        Just { rp | stockItems = { resource = o.resource, maybeOrderId = Just o.id } :: rp.stockItems }

    else
        Nothing


viewRoadPost : RoadPost -> Html msg
viewRoadPost rp =
    group [ styleMoveToGP rp.id ]
        [ circleWithDiameter
            roadEndpointDiameter
            [ stroke strokeColor
            , fill "#333"
            , strokeWidth strokeThickness
            ]
        , group []
            (List.indexedMap (\i -> .resource >> viewResource i) rp.stockItems)
        ]


viewResource : Int -> Resource -> Html msg
viewResource i resource =
    circleWithDiameter
        -- driverDiameter
        roadEndpointDiameter
        [ stroke strokeColor
        , (resourceColor >> fill) resource
        , strokeWidth strokeThickness
        , defaultTransition
        , styleTranslate ( 0, toFloat i * (-2 * strokeThickness) )
        ]



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
    { driverId : DriverId, from : Dest, to : Dest }


type Dest
    = DestBuilding BuildingRef
    | DestRoadPost { roadPostId : GP, gp : GP }



-- | DestRoadPost { id : GP, gp : GP }


orderMatchesBuildingId : BuildingId -> Order -> Bool
orderMatchesBuildingId bid o =
    o.from.id == bid || o.to.id == bid


orderMayBeAssignedToDriver : DriverId -> Order -> Bool
orderMayBeAssignedToDriver driverId o =
    (Pivot.getC o.steps :: Pivot.getR o.steps)
        |> List.any (.driverId >> eq driverId)


orderCurrentStep : Order -> Step
orderCurrentStep o =
    Pivot.getC o.steps


orderCurrentlyAssignedDriverId : Order -> DriverId
orderCurrentlyAssignedDriverId o =
    o |> orderCurrentStep |> .driverId


orderCurrentPickupGP : Order -> GP
orderCurrentPickupGP o =
    o |> orderCurrentStep |> .from |> destGP


orderCurrentDropOffGP : Order -> GP
orderCurrentDropOffGP o =
    o |> orderCurrentStep |> .to |> destGP


orderShouldWaitForDropoffCompletion : Order -> Bool
orderShouldWaitForDropoffCompletion o =
    case o |> orderCurrentStep |> .to of
        DestRoadPost _ ->
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

        DestRoadPost { gp } ->
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


driverNotifyHandoverCompleted : OrderId -> Driver -> Driver
driverNotifyHandoverCompleted orderId d =
    case d.state of
        WaitingForHandover o ->
            if orderId == o.id then
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

        WaitingForHandover o ->
            ( d, Just (DriverAtDropoffDest o) )

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


driverUpdateOnDriverDestroyed : DriverId -> Driver -> ( Orders, Driver )
driverUpdateOnDriverDestroyed driverId d =
    let
        ( removedOrders, pendingOrders ) =
            d.pendingOrders
                |> List.partition (orderMayBeAssignedToDriver driverId)
    in
    case driverCurrentOrder d |> ME.filter (orderMayBeAssignedToDriver driverId) of
        Just currentOrder ->
            ( currentOrder :: removedOrders
            , { d | pendingOrders = pendingOrders, state = Idle }
            )

        Nothing ->
            ( removedOrders
            , { d | pendingOrders = pendingOrders }
            )


driverUpdateOnBuildingDestroyed : BuildingId -> Driver -> ( Orders, Driver )
driverUpdateOnBuildingDestroyed bid d =
    let
        ( removedOrders, pendingOrders ) =
            d.pendingOrders
                |> List.partition (orderMatchesBuildingId bid)
    in
    case driverCurrentOrder d |> ME.filter (orderMatchesBuildingId bid) of
        Just currentOrder ->
            ( currentOrder :: removedOrders
            , { d | pendingOrders = pendingOrders, state = Idle }
            )

        Nothing ->
            ( removedOrders
            , { d | pendingOrders = pendingOrders }
            )


driverCurrentOrder : Driver -> Maybe Order
driverCurrentOrder d =
    case d.state of
        Idle ->
            Nothing

        WaitingForHandover o ->
            Just o

        PickingUp o ->
            Just o

        DroppingOff o ->
            Just o


viewKeyedDriver : Driver -> ( String, Html Msg )
viewKeyedDriver d =
    ( String.fromInt d.id, viewDriver d )


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
        [ styleMoveToGP gp
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
        , styleMoveToGP gp
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


driversUpdateOnDriverDestroyed : DriverId -> Drivers -> ( Orders, Drivers )
driversUpdateOnDriverDestroyed driverId drivers =
    drivers
        |> List.map (driverUpdateOnDriverDestroyed driverId)
        |> List.unzip
        |> Tuple.mapFirst List.concat


driversUpdateOnBuildingDestroyed : BuildingId -> Drivers -> ( Orders, Drivers )
driversUpdateOnBuildingDestroyed buildingId drivers =
    drivers
        |> List.map (driverUpdateOnBuildingDestroyed buildingId)
        |> List.unzip
        |> Tuple.mapFirst List.concat



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


type alias Building =
    { id : BuildingId
    , state : BuildingState
    , entry : GP
    , entryToCenterOffset : Float2
    , size : Int2
    , fill : String
    }


type alias BuildingId =
    Int


type alias ResourceQuantity =
    { resource : Resource, quantity : Int }


type BuildingState
    = UnderConstruction { inStock : InStock, initialProducer : Producer }
    | Producing Producer
    | NoProduction


buildingOccupiesGP : GP -> Building -> Bool
buildingOccupiesGP gp b =
    b.entry == gp || buildingBaseOccupiesGP gp b


buildingBaseOccupiesGP : GP -> Building -> Bool
buildingBaseOccupiesGP gp b =
    gpIntersectsSize (subBy2 (buildingLeftTopGP b) gp) b.size


buildingLeftTopGP : Building -> GP
buildingLeftTopGP b =
    tmap toFloat b.entry
        -- |> subBy2 b.entryToCenterOffset
        |> add2 b.entryToCenterOffset
        |> subBy2 (tmap (toFloat >> mul 0.5) b.size)
        |> tmap round
        |> Debug.log "debug"


gpIntersectsSize : GP -> Int2 -> Bool
gpIntersectsSize ( x, y ) ( w, h ) =
    x >= 0 && x < w && y >= 0 && y < h


buildingStep : Int -> Building -> Building
buildingStep _ b =
    case buildingStepState b.state of
        Nothing ->
            b

        Just newState ->
            { b | state = newState }


buildingStepState : BuildingState -> Maybe BuildingState
buildingStepState state =
    case state of
        UnderConstruction r ->
            if List.all inStockItemIsFilledAtCapacity r.inStock then
                Just (Producing r.initialProducer)

            else
                Nothing

        Producing producer ->
            producerStep producer
                |> Maybe.map Producing

        NoProduction ->
            Nothing


buildingInStock : Building -> InStock
buildingInStock b =
    case b.state of
        UnderConstruction r ->
            r.inStock

        Producing r ->
            r.inStock

        NoProduction ->
            []


buildingOutStock : Building -> OutStock
buildingOutStock b =
    case b.state of
        UnderConstruction r ->
            []

        Producing r ->
            r.outStock

        NoProduction ->
            []


buildingRequiredResources : Building -> List { required : Int, resource : Resource }
buildingRequiredResources b =
    b |> buildingInStock |> List.map inStockItemToRequirement


buildingAvailableOutResource : Resource -> Building -> Maybe Int
buildingAvailableOutResource resource b =
    case b.state of
        UnderConstruction r ->
            Nothing

        Producing producer ->
            producer.outStock
                |> LE.find (outStockItemIsResource resource)
                |> Maybe.map .availableAndUnreserved

        NoProduction ->
            Nothing


buildingRemoveOrderReservationsIfAny : Orders -> Building -> Building
buildingRemoveOrderReservationsIfAny orders b =
    List.foldl buildingRemoveOrderReservationIfAny b orders


buildingRemoveOrderReservationIfAny : Order -> Building -> Building
buildingRemoveOrderReservationIfAny o b =
    if o.from.id == b.id then
        buildingUpdateOutStockItemWithResource o.resource
            (outStockItemRemoveReservationIfAny o.id)
            b

    else if o.to.id == b.id then
        buildingUpdateInStockItemWithResource o.resource
            (inStockItemRemoveReservationIfAny o.id)
            b

    else
        b


buildingRegisterOutboundOrder : Order -> Building -> Building
buildingRegisterOutboundOrder o =
    buildingUpdateOutStockItemWithResource o.resource
        (outStockItemReserveForOrderId o.id)


buildingRegisterInboundOrder : Order -> Building -> Building
buildingRegisterInboundOrder o =
    buildingUpdateInStockItemWithResource o.resource
        (inStockItemReserveOrder o.id)


buildingUpdateOnOrderPickup : Resource -> OrderId -> Building -> Building
buildingUpdateOnOrderPickup resource orderId =
    buildingUpdateOutStockItemWithResource resource
        (outStockItemUpdateOnOrderPickup orderId)


buildingUpdateOnOrderDropoff : Resource -> OrderId -> Building -> Building
buildingUpdateOnOrderDropoff resource orderId =
    buildingUpdateInStockItemWithResource resource
        (inStockItemUpdateOnOrderDropoff orderId)


buildingUpdateInStockItemWithResource resource fn =
    mapState
        (\state ->
            case state of
                UnderConstruction r ->
                    r
                        |> mapInStock (updateExactlyOneWithResource resource fn)
                        |> UnderConstruction

                Producing r ->
                    r
                        |> mapInStock (updateExactlyOneWithResource resource fn)
                        |> Producing

                NoProduction ->
                    Debug.todo "why update stock in no production state"
        )


buildingUpdateOutStockItemWithResource resource fn =
    mapState
        (\state ->
            case state of
                UnderConstruction r ->
                    Debug.todo "cannot update OutStockItem of building underconstruction"

                Producing r ->
                    { r | outStock = updateExactlyOneWithResource resource fn r.outStock }
                        |> Producing

                NoProduction ->
                    Debug.todo "why update stock in no production state"
        )


mapInStock fn r =
    { r | inStock = fn r.inStock }


mapState fn b =
    { b | state = fn b.state }



-- BLUEPRINT


buildingFromBlueprintAt : Float2 -> Blueprint -> Building
buildingFromBlueprintAt point b =
    let
        entry =
            add2 (tscale -cellDiameter b.entryToCenterOffset) point
                |> worldToGP
    in
    { b | entry = entry }


blueprintRotate b =
    { b | entryToCenterOffset = rotateEntryToCenterOffset b.entryToCenterOffset, size = rotateSize b.size }


rotateEntryToCenterOffset ( x, y ) =
    ( x, y ) |> toPolar |> Tuple.mapSecond (add (turns 0.25)) |> fromPolar


rotateSize ( w, h ) =
    ( h, w )



-- BUILDING REF


type alias BuildingRef =
    { id : BuildingId, gp : GP }


buildingToRef : Building -> BuildingRef
buildingToRef b =
    { id = b.id, gp = b.entry }



-- PRODUCER


type alias Producer =
    { every : Int
    , inputs : List ResourceQuantity
    , output : Resource
    , elapsed : Int
    , inStock : InStock
    , outStock : OutStock
    }


producerStep : Producer -> Maybe Producer
producerStep ({ every, inputs, output, elapsed } as r) =
    stockPerformIO inputs output r.inStock r.outStock
        |> Maybe.map
            (\stock ->
                if elapsed + 1 >= every then
                    { r | elapsed = 0, inStock = stock.inStock, outStock = stock.outStock }

                else
                    { r | elapsed = elapsed + 1 }
            )


stockPerformIO : List ResourceQuantity -> Resource -> InStock -> OutStock -> Maybe { inStock : InStock, outStock : OutStock }
stockPerformIO inputs output inStock outStock =
    Maybe.map2 (\newInStock newOutStock -> { inStock = newInStock, outStock = newOutStock })
        (inStockConsumeInputs inputs inStock)
        (outStockStoreResource output outStock)



-- IN STOCK ITEM


type alias InStockItem =
    { totalCapacity : Int
    , resource : Resource
    , filledCapacity : Int
    , reservedCapacityForOrderIds : List OrderId
    }


initInStockItem : Int -> Resource -> InStockItem
initInStockItem capacity resource =
    { totalCapacity = capacity, resource = resource, filledCapacity = 0, reservedCapacityForOrderIds = [] }


inStockItemToRequirement : InStockItem -> { required : Int, resource : Resource }
inStockItemToRequirement r =
    { required = inStockItemFreeCapacity r, resource = r.resource }


inStockItemFreeCapacity : InStockItem -> Int
inStockItemFreeCapacity r =
    r.totalCapacity - (r.filledCapacity + List.length r.reservedCapacityForOrderIds)


inStockItemHasFreeCapacity : InStockItem -> Bool
inStockItemHasFreeCapacity r =
    inStockItemFreeCapacity r > 0


inStockItemReserveOrder : OrderId -> InStockItem -> InStockItem
inStockItemReserveOrder orderId r =
    if inStockItemHasFreeCapacity r then
        { r | reservedCapacityForOrderIds = orderId :: r.reservedCapacityForOrderIds }

    else
        Debug.todo "inStockItemReserveOrder failed"


inStockItemRemoveReservationIfAny : OrderId -> InStockItem -> InStockItem
inStockItemRemoveReservationIfAny orderId r =
    if List.member orderId r.reservedCapacityForOrderIds then
        { r | reservedCapacityForOrderIds = LE.remove orderId r.reservedCapacityForOrderIds }

    else
        r


inStockItemIsFilledAtCapacity : InStockItem -> Bool
inStockItemIsFilledAtCapacity r =
    r.totalCapacity == r.filledCapacity


inStockItemUpdateOnOrderDropoff : OrderId -> InStockItem -> InStockItem
inStockItemUpdateOnOrderDropoff orderId r =
    if List.member orderId r.reservedCapacityForOrderIds then
        { r
            | filledCapacity = r.filledCapacity + 1
            , reservedCapacityForOrderIds = LE.remove orderId r.reservedCapacityForOrderIds
        }

    else
        Debug.todo "order drop off failed: inStockItem not reserved for orderId"


inStockItemConsume : Int -> InStockItem -> Maybe InStockItem
inStockItemConsume quantity inStockItem =
    if .filledCapacity inStockItem >= quantity then
        Just { inStockItem | filledCapacity = inStockItem.filledCapacity - quantity }

    else
        Nothing



-- OUT STOCK ITEM


type alias OutStockItem =
    { totalCapacity : Int
    , resource : Resource
    , availableAndUnreserved : Int
    , availableAndReservedForOrderIds : List OrderId
    }


initOutStockItem : Int -> Resource -> OutStockItem
initOutStockItem capacity resource =
    { totalCapacity = capacity, resource = resource, availableAndUnreserved = 0, availableAndReservedForOrderIds = [] }


outStockItemFreeCapacity : OutStockItem -> Int
outStockItemFreeCapacity r =
    r.totalCapacity - (r.availableAndUnreserved + List.length r.availableAndReservedForOrderIds)


outStockItemIsResource : Resource -> OutStockItem -> Bool
outStockItemIsResource resource r =
    r.resource == resource


outStockItemUpdateOnOrderPickup : OrderId -> OutStockItem -> OutStockItem
outStockItemUpdateOnOrderPickup orderId r =
    if List.member orderId r.availableAndReservedForOrderIds then
        { r | availableAndReservedForOrderIds = LE.remove orderId r.availableAndReservedForOrderIds }

    else
        Debug.todo "outStockItemUpdateOnOrderPickup failed"


outStockItemProduce : OutStockItem -> Maybe OutStockItem
outStockItemProduce outStockItem =
    if outStockItemFreeCapacity outStockItem > 0 then
        Just { outStockItem | availableAndUnreserved = outStockItem.availableAndUnreserved + 1 }

    else
        Nothing


outStockItemReserveForOrderId : OrderId -> OutStockItem -> OutStockItem
outStockItemReserveForOrderId orderId r =
    if r.availableAndUnreserved > 0 && notMember orderId r.availableAndReservedForOrderIds then
        { r
            | availableAndUnreserved = r.availableAndUnreserved - 1
            , availableAndReservedForOrderIds = orderId :: r.availableAndReservedForOrderIds
        }

    else
        Debug.todo "outStockItem reservation failed"


outStockItemRemoveReservationIfAny : OrderId -> OutStockItem -> OutStockItem
outStockItemRemoveReservationIfAny orderId r =
    if List.member orderId r.availableAndReservedForOrderIds then
        { r | availableAndReservedForOrderIds = LE.remove orderId r.availableAndReservedForOrderIds }

    else
        r



-- IN STOCK


type alias InStock =
    List InStockItem


inStockConsumeInputs : List ResourceQuantity -> InStock -> Maybe InStock
inStockConsumeInputs inputs inStock =
    inputs
        |> List.foldl (\rq -> Maybe.andThen (inStockConsumeInput rq)) (Just inStock)


inStockConsumeInput : ResourceQuantity -> InStock -> Maybe InStock
inStockConsumeInput rq inStock =
    let
        newInStock =
            updateExactlyOneWithResource rq.resource
                (\inStockItem -> inStockItem |> inStockItemConsume rq.quantity |> Maybe.withDefault inStockItem)
                inStock
    in
    if inStock == newInStock then
        Nothing

    else
        Just newInStock



-- OUT STOCK


type alias OutStock =
    List OutStockItem


outStockStoreResource : Resource -> OutStock -> Maybe OutStock
outStockStoreResource resource outStock =
    let
        newStock =
            updateExactlyOneWithResource resource
                (\outStockItem -> outStockItemProduce outStockItem |> Maybe.withDefault outStockItem)
                outStock
    in
    if outStock == newStock then
        Nothing

    else
        Just newStock



-- BUILDING VIEW


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
            , viewStockViewModel (buildingStockViewModel b)
            ]
        , circleWithDiameter buildingEntryDiameter
            [ fill b.fill
            , stroke strokeColor
            , strokeWidth strokeThickness
            , styleMoveToGP b.entry
            ]

        -- , group [ styleMoveToGP (buildingLeftTopGP b) ]
        --     [ words "LT" [ fill "red" ] ]
        ]


type IO
    = In
    | Out


type alias StockItemViewModel =
    { io : IO, resource : Resource, stored : Int }


inStockItemViewModel : InStockItem -> StockItemViewModel
inStockItemViewModel r =
    { io = In, resource = r.resource, stored = r.filledCapacity }


outStockItemViewModel : OutStockItem -> StockItemViewModel
outStockItemViewModel r =
    { io = Out
    , resource = r.resource
    , stored = r.availableAndUnreserved + List.length r.availableAndReservedForOrderIds
    }


type alias StockViewModel =
    List StockItemViewModel


buildingStockViewModel : Building -> StockViewModel
buildingStockViewModel b =
    List.map inStockItemViewModel (buildingInStock b)
        ++ List.map outStockItemViewModel (buildingOutStock b)


viewStockViewModel : StockViewModel -> Html msg
viewStockViewModel stockViewModel =
    let
        viewStockItemViewModel i sivm =
            let
                string =
                    "$IO $RESOURCE $STORED"
                        |> String.replace "$IO" (Debug.toString sivm.io)
                        |> String.replace "$RESOURCE" (Debug.toString sivm.resource)
                        |> String.replace "$STORED" (Debug.toString sivm.stored)
            in
            group [ styleTranslate ( 0, toFloat i * cellGap ) ]
                [ words string [ fill strokeColor, style "font-size" "13px", style "font-family" "monospace" ] ]
    in
    group [ styleTranslate ( 0, -cellGap * 2 ) ] (List.indexedMap viewStockItemViewModel stockViewModel)



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


notMember x =
    List.member x >> not


findExactlyOne pred list =
    case List.filter pred list of
        only :: [] ->
            only

        _ ->
            Debug.todo "findExactlyOne failed"


reject =
    LE.filterNot


allPass preds val =
    List.map (\fn -> fn val) preds
        |> List.foldl (&&) True


sum2 =
    List.foldl add2 ( 0, 0 )


subBy2 =
    tmap2 subBy


subBy b a =
    a - b


applyN n fn a =
    List.repeat n fn |> List.foldl (<|) a


atLeast =
    max


atMost =
    min


neq =
    (/=)


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
