module LiveChat where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import StartApp.Simple as StartApp

-- MODEL

type alias Consultation =
  { id: Int,
    state: String,
    status: String,
    specialty: String,
    member_id: Int,
    member_name: String }

type alias Provider =
  { id: Int,
    name: String,
    state: String,
    specialty: String }

type alias Member =
  { id: Int,
    name: String }

type alias CreateConsultPayload =
  { id: Int,
    state: String,
    status: String,
    specialty: String,
    member_id: Int,
    member_name: String }

--type alias LockConsultPayload =
--  { id: Int,
--    provider_id: Int }
--
--type alias CompleteConsultPayload =
--  { id: Int,
--    provider_id: Int }
--
--type alias CancelConsultPayload =
--  { id: Int }

type alias ActionString = String

type alias CreateConsultEvent =
  { action: ActionString,
    payload: CreateConsultPayload }

--type alias LockConsultEvent =
--  { action: ActionString,
--    payload: LockConsultPayload }
--
--type alias CompleteConsultEvent =
--  { action: ActionString,
--    payload: CompleteConsultPayload }
--
--type alias CancelConsultEvent =
--  { action: ActionString,
--    payload: CancelConsultPayload }

--type Event = CreateConsultEvent
--           | LockConsultEvent
--           | CompleteConsultEvent
--           | CancelConsultEvent

--type EventPayload = CreateConsultPayload
--                  | LockConsultPayload
--                  | CompleteConsultPayload
--                  | CancelConsultPayload

type alias EventActionCode = String

--type alias EventActionMessage = { action: EventActionCode,
--                                  payload: EventPayload }

type alias Model =
  { consultations: List Consultation,
    providers: List Provider,
    members: List Member,
    author: String,
    action_string : String,
    newMessage : String,
    messages : List String,
    outgoingMessage : String }

initialConsultation1 : Consultation
initialConsultation1 =
  { id = 31,
    state = "CA",
    status = "Completed",
    specialty = "General Medical",
    member_id = 23,
    member_name = "Stan Geldoff" }

initialConsultation2 : Consultation
initialConsultation2 =
  { id = 47,
    state = "NY",
    status = "Requested",
    specialty = "Sexual Health",
    member_id = 546,
    member_name = "Philip Vanderhoff" }

initialConsultation3 : Consultation
initialConsultation3 =
  { id = 164,
    state = "NM",
    status = "Locked",
    specialty = "Behavioral Health",
    member_id = 924,
    member_name = "James McDoogle" }

initialConsultation4 : Consultation
initialConsultation4 =
  { id = 92,
    state = "LA",
    status = "Requested",
    specialty = "Sexual Health",
    member_id = 1032,
    member_name = "Melissa McCartney" }

initialConsultation5 : Consultation
initialConsultation5 =
  { id = 320,
    state = "NC",
    status = "Locked",
    specialty = "General Medical",
    member_id = 204,
    member_name = "Diana Smith" }

consultationsList = [ initialConsultation1, initialConsultation2, initialConsultation3, initialConsultation4, initialConsultation5 ]

initialModel : Model
initialModel =
  { consultations = consultationsList,
    providers = [],
    members = [],
    author = "Geoffrey",
    action_string = "",
    newMessage = "",
    messages = [],
    outgoingMessage = "" }

type alias OriginalModel =
  { author : String,
    newMessage : String,
    messages : List String,
    outgoingMessage : String }

messageFromModel : Model -> String
messageFromModel model =
  "<" ++ model.author ++ "> " ++ model.newMessage

-- UPDATE
type Action = NoOp
            | Add String
            | UpdateMessage String
            | UpdateAuthor String
            | NewEvent CreateConsultEvent
            | Send
--            | NewEvent LockConsultEvent
--            | NewEvent CompleteConsultEvent
--            | NewEvent CancelConsultEvent

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Add message ->
      { model | messages = model.messages ++ [message] }
    UpdateMessage message ->
      { model | newMessage = message }
    UpdateAuthor author ->
      { model | author = author }
    NewEvent event ->
      { model | consultations = (event.payload :: model.consultations) }
--    NewEvent LockConsultEvent ->
--      model
--    NewEvent CompleteConsultEvent ->
--      model
--    NewEvent CancelConsultEvent ->
--      model
    Send ->
      { model |
        outgoingMessage = messageFromModel model,
        newMessage = ""
      }

-- VIEW
showMessage : String -> Html
showMessage msg =
  div [ class "message col-xs-12" ] [ text msg ]

requestedConsultStyle : Attribute
requestedConsultStyle =
  style
    [ ("backgroundColor", "paleturquoise")
    , ("color", "purple")
    , ("width", "100%") ]

lockedConsultStyle : Attribute
lockedConsultStyle =
  style
    [ ("backgroundColor", "mistyrose")
    , ("color", "darkred")
    , ("width", "100%") ]

completedConsultStyle : Attribute
completedConsultStyle =
  style
    [ ("backgroundColor", "blanchedalmond")
    , ("width", "100%") ]

consultStyle : Consultation -> Attribute
consultStyle consult =
  if consult.status == "Completed" then
    completedConsultStyle
  else if consult.status == "Requested" then
    requestedConsultStyle
  else if consult.status == "Locked" then
    lockedConsultStyle
  else
    lockedConsultStyle

buttonText : Consultation -> String
buttonText consult =
  if consult.status == "Completed" then
    ""
  else if consult.status == "Requested" then
    "Lock"
  else if consult.status == "Locked" then
    "Unlock"
  else
    ""

showConsultation : Consultation -> Html
showConsultation consult =
  div [] [
    span [ class "consultation col-xs-12", (consultStyle consult) ] [
      div [] [ text ("Consultation ID: " ++ (toString consult.id) ++ " / State: " ++ consult.state)],
      div [] [ text ("Status: " ++ consult.status ++ " / Specialty: " ++ consult.specialty)],
      div [] [ text ("Member: " ++ consult.member_name ++ " (ID: " ++ (toString consult.member_id) ++ ")")],
      div [] [ (if consult.status /= "Completed" then (button [] [ text (buttonText consult) ]) else br [] []) ]
    ]
  ]

entryForm : Signal.Address Action -> Model -> Html
entryForm address model =
  div [ class "chat-form row" ] [
    div [ class "col-xs-2"] [
      input [
        type' "text",
        placeholder "Guest",
        class "author-entry form-control col-xs-2",
        value model.author,
        on "input" targetValue (Signal.message address << UpdateAuthor),
        id "author-input"
      ] [ ]
    ],
    div [ class "col-xs-8" ] [
      input [
        type' "text",
        placeholder "Enter your message here...",
        class "message-entry form-control",
        value model.newMessage,
        on "input" targetValue (Signal.message address << UpdateMessage),
        id "chat-input"
      ] [ ]
    ],
    div [ class "col-xs-2 text-right" ] [
      button [ class "btn btn-primary", onClick address Send ] [ text "Send" ]
    ]
  ]

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [
      div [ class "display-chat" ] [
        h2 [] [ text "WELCOME TO ELM" ],
        div [ class "chat-log row col-xs-12" ] (List.map showMessage model.messages),
        entryForm address model
      ],
      div [ class "output"] [
        h2 [] [ text "CONSULTATIONS" ],
        div [ ] (List.map showConsultation model.consultations)
      ]
    ]

ticker : Signal.Signal Int
ticker =
  Signal.foldp (\_ val -> val + 1) 0 (Time.every Time.second)

newConsultSignal : Signal.Signal (Maybe Consultation)
newConsultSignal =
  Signal.map (\int -> (List.head (List.reverse (List.take (int + 1) consultationsList)))) ticker

--addConsultation : Signal.Signal Consultation -> Model -> Model

-- PORTS
port incomingMessages : Signal String

port outgoingMessage : Signal String
port outgoingMessage =
  Signal.dropRepeats (Signal.map .outgoingMessage modelSignal)

-- MAIN
inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp

--actions : Signal Action
--actions =
--  Signal.mergeMany [inbox.signal, (Signal.dropRepeats (Signal.map Add incomingMessages)), newConsultSignal]

actions : Signal (Maybe Consultation)
actions =
  newConsultSignal

modelSignal : Signal Model
modelSignal =
  Signal.foldp update initialModel actions

main : Signal Html
main =
--  Signal.map (view inbox.address) model
  Signal.map (view inbox.address) modelSignal
