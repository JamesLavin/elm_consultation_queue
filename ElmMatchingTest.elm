
module LiveChat where

import Html exposing (..)
--import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
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

type alias Model =
  { consultations: List Consultation,
    providers: List Provider,
    members: List Member,
    author: String,
    action_string : String,
    newMessage : String,
    messages : List String,
    outgoingMessage : String }

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

type Action =
  NewEvent (JsonEvent {})

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

--update : Action -> Model -> Model
--update action model =
--  case action of
--    NewEvent payload -> process_new_event payload model

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
    span [ class "consultation", (consultStyle consult) ] [
      div [] [ text ("Consultation ID: " ++ (toString consult.id) ++ " / State: " ++ consult.state)],
      div [] [ text ("Status: " ++ consult.status ++ " / Specialty: " ++ consult.specialty)],
      div [] [ text ("Member: " ++ consult.member_name ++ " (ID: " ++ (toString consult.member_id) ++ ")")],
      div [] [ (if consult.status /= "Completed" then (button [] [ text (buttonText consult) ]) else br [] []) ]
    ]
  ]

--allConsultations : Model -> VirtualDom.Node
allConsultations model =
  List.map showConsultation model.consultations

requestedConsultations model =
  List.map showConsultation (List.filter (\consult -> consult.status == "Requested") model.consultations)

completedConsultations model =
  List.map showConsultation (List.filter (\consult -> consult.status == "Completed") model.consultations)

view : Model -> Html
view model =
  div []
    [
      img [ src "Teladoc_logo.jpg", style [ ("width", "150px"), ("height", "150px") ] ] []
    ,
      div [ class "header" ] [
        h1 [] [ text "Super Amazingly Awesome Teladoc Consultation Queue" ]
      ]
    ,
      div [] [
        span [ class "consultations", style [ ("width", "33%") ] ] [
          h2 [] [ text "CONSULTATIONS" ],
          div [ ] (allConsultations model)
        ]
      ,
        span [ class "consultations", style [ ("width", "33%") ] ] [
          h2 [] [ text "REQUESTED" ],
          div [ ] (requestedConsultations model)
        ]
      ,
        span [ class "consultations", style [ ("width", "33%") ] ] [
          h2 [] [ text "COMPLETED" ],
          div [ ] (completedConsultations model)
        ]
      ]
    ]


main =
  view initialModel
