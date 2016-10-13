module Main exposing ( .. )

import Html.App
import Html as H
-- import Debug

import Login
import Propane

type Page = Login | Propane

type alias Model = { currPage : Page
                   , loginModel : Login.Model
                   , propaneModel : Propane.Model
                   }

initialModel : Model
initialModel = { currPage = Login
               , loginModel = Login.initialModel
               , propaneModel = Propane.initialModel
               }

type Msg = ChangePage Page
         | LoginAction Login.Msg
         | PropaneAction Propane.Msg

changePage : Model -> Page -> Model
changePage model page = { model | currPage = page }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangePage page ->
            ( changePage model page, Cmd.none )
        LoginAction loginMsg ->
            let
                (loginModel, loginCmds) = Login.update loginMsg model.loginModel
                loggedInModel = { model | loginModel = loginModel }
                propaneInit = Cmd.map PropaneAction Propane.loginInit
            in
                case loginMsg of
                    Login.LoginSuccess token ->
                        ( changePage loggedInModel Propane
                        , Cmd.batch [ loginCmds |> Cmd.map LoginAction
                                    , propaneInit])
                    _ ->
                        ( loggedInModel, Cmd.map LoginAction loginCmds )
        PropaneAction propaneMsg ->
            let
                (propaneModel, propaneCmds) = Propane.update propaneMsg model.propaneModel
                propanedModel = { model | propaneModel = propaneModel }
                loginInit = Cmd.map LoginAction Login.loginInit
            in
                case propaneMsg of
                    Propane.LogoutSuccess token ->
                        ( changePage propanedModel Login
                        , Cmd.batch [ Cmd.map PropaneAction propaneCmds, loginInit ]
                        )
                    _ ->
                        ( propanedModel
                        , Cmd.map PropaneAction propaneCmds )

view : Model -> H.Html Msg
view model =
    case model.currPage of
        Login -> Login.view model.loginModel |> Html.App.map LoginAction
        Propane -> Propane.view model.propaneModel |> Html.App.map PropaneAction

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currPage of
        Login -> Sub.map LoginAction (Login.subscriptions model.loginModel)
        Propane -> Sub.map PropaneAction (Propane.subscriptions model.propaneModel)

main : Program Never
main =
    Html.App.program { init = (initialModel, Cmd.none)
                     , update = update
                     , view = view
                     , subscriptions = subscriptions
                     }
