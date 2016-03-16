-- Copyright 2016 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp as StartApp
import ElmFire.Auth as Auth
import ElmFire
import Effects
import Task
import Json.Decode as JD

app = StartApp.start { init = init, view = view, update = update, inputs = [ authMailbox.signal ] }

type State = LoggedOut | LoggedIn { uid : String, name : String }

init = (LoggedOut, Effects.none)

view address model =
  case model of
    LoggedOut ->
      button [ onClick address LogIn ] [ text "log in" ]
    LoggedIn {uid, name} ->
      div []
        [ div [] [text ("uid: " ++ uid ++ ", name: " ++ name)]
        , button [ onClick address LogOut ] [text "log out"]
        ]

type Action = NoOp | LogIn | LogOut | AuthAction (Maybe Auth.Authentication)

displayName value = Result.withDefault "" (JD.decodeValue (JD.at ["displayName"] JD.string) value)

update action model =
  case action of
    NoOp -> (model, Effects.none)
    LogIn -> (model, authenticateEffect)
    LogOut -> (model, unauthenticateEffect)
    AuthAction (Just {uid, specifics}) -> (LoggedIn {uid=uid, name=displayName specifics }, Effects.none)
    AuthAction Nothing -> (LoggedOut, Effects.none)

location = (ElmFire.fromUrl "https://nouvellestar.firebaseio.com/")

authenticate = Auth.authenticate location [] (Auth.withOAuthRedirect "google")

unauthenticate = Auth.unauthenticate location

authenticateEffect = authenticate
  |> Task.toMaybe
  |> Task.map (always NoOp)
  |> Effects.task

unauthenticateEffect = unauthenticate
  |> Task.toMaybe
  |> Task.map (always NoOp)
  |> Effects.task

authMailbox = Signal.mailbox NoOp

onAuth auth = Signal.send authMailbox.address (AuthAction auth)

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

port subscribe : Task.Task ElmFire.Error ()
port subscribe = Auth.subscribeAuth onAuth location

main = app.html
