module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Attribute, Html, button, div, span, text)
import Html.Attributes as HA exposing (class, style)
import Html.Events as HE exposing (onClick)
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


cellSize =
    ( 50, 50 )


allGPs =
    let
        ( w, h ) =
            ( 10, 10 )
    in
    times h (\y -> times w (\x -> ( x, y )))
        |> List.concat


toCellCenter gp =
    gp |> tmap toFloat |> tmap2 mul cellSize


view : Model -> Html Msg
view model =
    div []
        [ globalStyles
        , div [ style "padding" "20px" ]
            [ Svg.svg
                [ SA.viewBox "0 0 500 500"
                , style "width" "500px"
                , style "overflow" "visible"
                , stroke "none"
                , fill "none"
                ]
                (allGPs
                    |> List.map viewCell
                )
            ]
        ]


viewCell gp =
    rect
        (cellSize |> tmap (mul 0.9))
        [ stroke "#666"
        , styleTranslate (toCellCenter gp)
        , SA.rx "5"
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


rect ( w, h ) attrs =
    Svg.rect
        (SA.width (String.fromFloat w)
            :: SA.height (String.fromFloat h)
            :: attrs
        )
        []



-- BASICS


times n fn =
    List.range 0 (n - 1) |> List.map fn


mul =
    (*)


tmap fn =
    Tuple.mapBoth fn fn


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
