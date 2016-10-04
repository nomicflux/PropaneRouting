module Login exposing (..)

import Html.App
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Maybe exposing (Maybe(..))

type alias Model = { username : Maybe String
                   , password : Maybe String
                   }

initialModel : Model
initialModel = { username = Nothing
               , password = Nothing
               }

type Msg = Login
         | LoginSuccess String
         | LoginFailure String
         | InputUsername String
         | InputPassword String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            (model, Cmd.none)
        LoginSuccess success ->
            (model, Cmd.none)
        LoginFailure failure ->
            (model, Cmd.none)
        InputUsername uname ->
            ({model|username=Just uname}, Cmd.none)
        InputPassword pwd ->
            ({model|password=Just pwd}, Cmd.none)

usernameField : H.Html Msg
usernameField =
    H.span
        [ HA.class "input" ]
        [ H.label [HA.for "username" ] [H.text "Username: "]
        , H.input [HA.type' "text", HA.name "username", HE.onInput InputUsername] []
        ]

passwordField : H.Html Msg
passwordField =
    H.span
        [ HA.class "input" ]
        [ H.label [HA.for "password" ] [H.text "Password: "]
        , H.input [HA.type' "password", HA.name "password", HE.onInput InputPassword] []
        ]

view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class "login jumbotron" ]
        [ H.div
              [ ]
              [ usernameField
              , passwordField
              ]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


main : Program Never
main =
    Html.App.program { init = (initialModel, Cmd.none)
                     , update = update
                     , view = view
                     , subscriptions = subscriptions
                     }
