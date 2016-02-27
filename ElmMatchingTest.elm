
module LiveChat where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetChecked)
import Time exposing (..)
import Signal exposing (Address, foldp)
import Array
--import Random exposing (int)
--import StartApp.Simple as StartApp

type alias Provider =
  { id: Int,
    name: String,
    state: String,
    specialty: String }

type alias Member =
  { id: Int,
    name: String }

type alias Consultation =
  { id: Int,
    state: String,
    status: String,
    specialty: String,
    member_id: Int,
    member_name: String }

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
    member_name = "Darth Vader" }

initialConsultation4 : Consultation
initialConsultation4 =
  { id = 92,
    state = "LA",
    status = "Requested",
    specialty = "Sexual Health",
    member_id = 1032,
    member_name = "R2D2" }

initialConsultation5 : Consultation
initialConsultation5 =
  { id = 320,
    state = "NC",
    status = "Locked",
    specialty = "General Medical",
    member_id = 204,
    member_name = "Clark Kent" }

initialConsultation6 : Consultation
initialConsultation6 =
  { id = 61,
    state = "FL",
    status = "Cancelled",
    specialty = "General Medical",
    member_id = 894,
    member_name = "Bruce Wayne" }

consultationsList : List Consultation
consultationsList = [ initialConsultation1,
                      initialConsultation2,
                      initialConsultation3,
                      initialConsultation4,
                      initialConsultation5,
                      initialConsultation6 ]

type alias Model =
  { consultations: List Consultation,
    providers: List Provider,
    members: List Member,
    displayedQueues: List String,
    displayRequested: Bool
  }

initialModel : Model
initialModel =
  { consultations = consultationsList,
    providers = [],
    members = [],
    displayedQueues = ["Requested", "Locked", "Cancelled", "Completed"],
    displayRequested = True
  }

type Action = NoOp
--            | NewEvent (JsonEvent {})
--            | DisplayRequested Bool

type alias ConsultationPayload a =
  { a | id : Int }

type alias JsonEvent a =
  { a | action : String
  ,     payload : ConsultationPayload {} }

--type Payload = CreateConsultationPayload
--             | LockConsultationPayload
--             | CancelConsultationPayload
--             | CompleteConsultationPayload

type alias CreateConsultationPayload = Consultation

type alias LockConsultationPayload =
  { id: Int
  , provider_id: Int }

addDisplayRequested model =
  if (List.member "Requested" model.displayedQueues) then
    model
  else
    { model | displayedQueues = "Requested" :: model.displayedQueues }

removeFromList i xs =
  (List.take i xs) ++ (List.drop (i+1) xs)

removeDisplayRequested model =
  if (List.member "Requested" model.displayedQueues) then
    { model | displayedQueues = List.filter (\displayedQueue -> displayedQueue /= "Requested") model.displayedQueues }
  else
    model

--update : Action -> Model -> Model
update action model =
  case action of
--    NewEvent payload ->
--      process_new_event payload model
--    DisplayRequested boolean ->
--      model
    NoOp ->
      model

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp

process_new_event event model =
  case event.action of
    "CREATE_CONSULTATION" ->
      { model | consultations = event.payload :: model.consultations }
    _ ->
      model

requestedConsultStyle : Attribute
requestedConsultStyle =
  style
    [ ("backgroundColor", "paleturquoise")
    , ("color", "purple")
    , ("width", "100%")
    , ("margin", "5px")
    , ("border-style", "solid") ]

cancelledConsultStyle : Attribute
cancelledConsultStyle =
  style
    [ ("backgroundColor", "palegreen")
    , ("color", "green")
    , ("width", "100%")
    , ("margin", "5px")
    , ("border-style", "solid") ]

lockedConsultStyle : Attribute
lockedConsultStyle =
  style
    [ ("backgroundColor", "mistyrose")
    , ("color", "darkred")
    , ("margin", "5px")
    , ("width", "100%")
    , ("border-style", "solid") ]

completedConsultStyle : Attribute
completedConsultStyle =
  style
    [ ("backgroundColor", "blanchedalmond")
    , ("margin", "5px")
    , ("width", "100%")
    , ("border-style", "solid") ]

consultStyle : Consultation -> Attribute
consultStyle consult =
  if consult.status == "Completed" then
    completedConsultStyle
  else if consult.status == "Requested" then
    requestedConsultStyle
  else if consult.status == "Locked" then
    lockedConsultStyle
  else if consult.status == "Cancelled" then
    cancelledConsultStyle
  else
    lockedConsultStyle

consultQueueStyle : Attribute
consultQueueStyle =
  style
    [ ("width", "21%"), ("display", "inline-block"), ("margin", "1%"), ("vertical-align", "text-top") ]

buttonText : Consultation -> String
buttonText consult =
  if consult.status == "Completed" then
    ""
  else if consult.status == "Requested" then
    "Lock"
  else if consult.status == "Locked" then
    "Unlock"
  else if consult.status == "Cancelled" then
    ""
  else
    ""
showConsultation : Consultation -> Html
showConsultation consult =
  div [ (consultStyle consult) ] [
    span [ class "consultation" ] [
      div [] [ text ("Consultation ID: " ++ (toString consult.id) ++ " / State: " ++ consult.state)],
      div [] [ text ("Status: " ++ consult.status ++ " / Specialty: " ++ consult.specialty)],
      div [] [ text ("Member: " ++ consult.member_name ++ " (ID: " ++ (toString consult.member_id) ++ ")")],
      div [] [ (if (consult.status /= "Completed" && consult.status /= "Cancelled") then (button [] [ text (buttonText consult) ]) else br [] []) ]
    ]
  ]

ticker : Signal.Signal Int
ticker =
  Signal.foldp (\_ val -> val + 1) 0 (Time.every Time.second)

--allConsultations : Model -> VirtualDom.Node
allConsultations model =
  List.map showConsultation model.consultations

filteredConsultations model status =
  List.filter (\consult -> consult.status == status) model.consultations

showFilteredConsultations model status =
  List.map showConsultation (filteredConsultations model status)

teladocImg =
  img [ src "Teladoc_logo.jpg", style [ ("width", "150px"), ("height", "150px") ] ] []

teladocHeadline =
  div [ class "header" ] [
    h1 [] [ text "Super Amazingly Awesome Teladoc Consultation Queue" ]
  ]

--teladocConsultationQueues model =
--  div [] [
--    span [ class "consultations", consultQueueStyle ] [
--      h2 [] [ text "REQUESTED" ],
--      div [ ] (showFilteredConsultations model "Requested")
--    ]
--  ,
--    span [ class "consultations", consultQueueStyle ] [
--      h2 [] [ text "LOCKED" ],
--      div [ ] (showFilteredConsultations model "Locked")
--    ]
--  ,
--    span [ class "consultations", consultQueueStyle ] [
--      h2 [] [ text "CANCELLED" ],
--      div [ ] (showFilteredConsultations model "Cancelled")
--    ]
--  ,
--    span [ class "consultations", consultQueueStyle ] [
--      h2 [] [ text "COMPLETED" ],
--      div [ ] (showFilteredConsultations model "Completed")
--    ]
--  ]

classConsultSpan model class =
  case class of
    "Requested" ->
      requestedConsultSpan model
    "Locked" ->
      lockedConsultSpan model
    "Cancelled" ->
      cancelledConsultSpan model
    "Completed" ->
      completedConsultSpan model
    _ ->
      completedConsultSpan model

requestedConsultSpan model =
  span [ class "consultations", consultQueueStyle ] [
    h2 [] [ text "REQUESTED" ],
    div [ ] (showFilteredConsultations model "Requested")
  ]

lockedConsultSpan model =
  span [ class "consultations", consultQueueStyle ] [
    h2 [] [ text "LOCKED" ],
    div [ ] (showFilteredConsultations model "Locked")
  ]

cancelledConsultSpan model =
  span [ class "consultations", consultQueueStyle ] [
    h2 [] [ text "CANCELLED" ],
    div [ ] (showFilteredConsultations model "Cancelled")
  ]

completedConsultSpan model =
  span [ class "consultations", consultQueueStyle ] [
    h2 [] [ text "COMPLETED" ],
    div [ ] (showFilteredConsultations model "Completed")
  ]

--teladocOptions address model =
--  div [] [
--    checkbox address model.displayRequested DisplayRequested "Show requested"
--  ]

teladocConsultationQueues model =
  div [] [
    div [] (List.map (classConsultSpan model) model.displayedQueues )
  ]

checkbox : Address Action -> Bool -> (Bool -> Action) -> String -> List Html
checkbox address isChecked tag name =
  [ input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message address << tag)
      ]
      []
  , text name
  , br [] []
  ]

view : Address Action -> Model -> Html
view address model =
  div []
    [ teladocImg
    , teladocHeadline
--    , teladocOptions address model
    , teladocConsultationQueues model
    ]

modelSignal : Signal Model
modelSignal =
  Signal.foldp update initialModel inbox.signal

main =
  Signal.map (view inbox.address) modelSignal
