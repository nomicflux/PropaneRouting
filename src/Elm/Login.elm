module Login exposing (..)

import Html.App
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Maybe exposing (Maybe(..), withDefault)
import Task
import Token

import Debug

import LoginAPI

type alias Model = { username : Maybe String
                   , password : Maybe String
                   , error : Maybe String
                   }

initialModel : Model
initialModel = { username = Nothing
               , password = Nothing
               , error = Nothing
               }

type Msg = Login
         | LoginSuccess (Maybe Token.Token)
         | LoginFailure Http.Error
         | LogoutSuccess (Maybe Token.Token)
         | LogoutFailure Http.Error
         | InputUsername String
         | InputPassword String

addError : Model -> String -> Model
addError model err = { model | error = Just err }

removeError : Model -> Model
removeError model = { model | error = Nothing }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login ->
            let
                user = { username = withDefault "" model.username
                       , password = withDefault "" model.password
                       }
            in
                (removeError model, LoginAPI.login user |> Task.perform LoginFailure LoginSuccess)
        LoginSuccess success ->
            let
                _ = success |> Debug.log "Success"
            in
                (removeError model, Cmd.none)
        LoginFailure failure ->
            let
                _ = failure |> Debug.log "Failure"
                body = case failure of
                           Http.BadResponse _ err -> err
                           Http.UnexpectedPayload err -> err
                           _ -> "Unknown error"
            in
                (addError model ("Error: " ++ body), Cmd.none)
        InputUsername uname ->
            (removeError {model|username=Just uname}, Cmd.none)
        InputPassword pwd ->
            (removeError {model|password=Just pwd}, Cmd.none)
        LogoutSuccess success ->
            let
                _ = success |> Debug.log "Success"
            in
                (removeError model, Cmd.none)
        LogoutFailure failure ->
            let
                _ = failure |> Debug.log "Failure"
                body = case failure of
                           Http.BadResponse _ err -> err
                           Http.UnexpectedPayload err -> err
                           _ -> "Unknown error"
            in
                (addError model ("Error: " ++ body), Cmd.none)

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

errorDiv : Model -> List (H.Html Msg)
errorDiv model =
    case model.error of
        Nothing -> []
        Just err -> [ H.aside [ HA.class "pure-alert pure-alert-error" ] [ H.text err ] ]

view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class "login jumbotron" ]
        ( H.div
              [ HA.class "pure-form pure-form-stacked" ]
              [ usernameField
              , passwordField
              , H.button
                  [ HA.class "pure-button pure-button-primary", HE.onClick Login ]
                  [ H.text "Login" ]
              ] :: errorDiv model)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

loginInit : Cmd Msg
loginInit = Cmd.none -- Task.perform LogoutFailure LogoutSuccess LoginAPI.logout

main : Program Never
main =
    Html.App.program { init = (initialModel, loginInit)
                     , update = update
                     , view = view
                     , subscriptions = subscriptions
                     }
