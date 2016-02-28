
module ElmConsultQueue where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetChecked)
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
  { consultations: List Consultation
  , providers: List Provider
  , members: List Member
  , displayedQueues: List String
  , displayCancelled: Bool
  , displayCompleted: Bool
  , filterState: String
  }

initialModel : Model
initialModel =
  { consultations = consultationsList
  , providers = []
  , members = []
  , displayedQueues = ["Cancelled", "Requested", "Locked", "Completed"]
  , displayCancelled = True
  , displayCompleted = True
  , filterState = ""
  }

type Action = NoOp
            | DisplayCancelled Bool
            | DisplayCompleted Bool
            | FilterState String
            | NewEvent (JsonEvent {})
            | Lock Consultation
            | Complete Consultation

type alias ConsultationPayload a =
  { a | id : Int
      , state: String
      , status: String
      , specialty: String
      , member_id: Int
      , member_name: String }

type alias JsonEvent a =
  { a | action : String
  ,     payload : ConsultationPayload {} }

--type Payload = ConsultationPayload
--             | LockConsultationPayload
--             | CancelConsultationPayload
--             | CompleteConsultationPayload

type alias CreateConsultationPayload = Consultation

type alias LockConsultationPayload =
  { id: Int
  , provider_id: Int }

-- removeFromList i xs =
--   (List.take i xs) ++ (List.drop (i+1) xs)

addDisplayCancelled model =
  if (List.member "Cancelled" model.displayedQueues) then
    { model | displayCancelled = True }
  else
    { model | displayCancelled = True
            , displayedQueues = "Cancelled" :: model.displayedQueues }

removeDisplayCancelled model =
  if (List.member "Cancelled" model.displayedQueues) then
    { model | displayCancelled = False
            , displayedQueues = List.filter (\displayedQueue -> displayedQueue /= "Cancelled") model.displayedQueues }
  else
    { model | displayCancelled = True }

setDisplayCancelled model boolean =
  if boolean then
    addDisplayCancelled model
  else
    removeDisplayCancelled model

addDisplayCompleted model =
  if (List.member "Completed" model.displayedQueues) then
    { model | displayCompleted = True }
  else
    { model | displayCompleted = True
            , displayedQueues = List.append model.displayedQueues ["Completed"] }

removeDisplayCompleted model =
  if (List.member "Completed" model.displayedQueues) then
    { model | displayCompleted = False
            , displayedQueues = List.filter (\displayedQueue -> displayedQueue /= "Completed") model.displayedQueues }
  else
    { model | displayCompleted = True }

setDisplayCompleted model boolean =
  if boolean then
    addDisplayCompleted model
  else
    removeDisplayCompleted model

update : Action -> Model -> Model
update action model =
  case action of
    NewEvent payload ->
      process_new_event payload model
    DisplayCancelled boolean ->
      setDisplayCancelled model boolean
    DisplayCompleted boolean ->
      setDisplayCompleted model boolean
    FilterState state ->
      { model | filterState = state }
    Lock consult ->
      let
        this_consult_id = consult.id
        convert_consultation consultation =
          if consultation.id == this_consult_id then
            { consultation | status = "Locked" }
          else
            consultation
      in
        { model | consultations = List.map convert_consultation model.consultations }
    Complete consult ->
      let
        this_consult_id = consult.id
        convert_consultation consultation =
          if consultation.id == this_consult_id then
            { consultation | status = "Completed" }
          else
            consultation
      in
        { model | consultations = List.map convert_consultation model.consultations }
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

consultQueueStyle : Model -> Attribute
consultQueueStyle model =
  let
    width = case (List.length model.displayedQueues) of
      4 -> "21%"
      3 -> "28%"
      2 -> "42%"
      _ -> "21%"
  in
    style
      [ ("width", width), ("display", "inline-block"), ("margin", "1%"), ("vertical-align", "text-top") ]

buttonText : Consultation -> String
buttonText consult =
  if consult.status == "Completed" then
    ""
  else if consult.status == "Requested" then
    "Lock"
  else if consult.status == "Locked" then
    "Complete"
  else if consult.status == "Cancelled" then
    ""
  else
    ""

buttonAction : Consultation -> Action
buttonAction consult =
  if consult.status == "Completed" then
    NoOp
  else if consult.status == "Requested" then
    Lock consult
  else if consult.status == "Locked" then
    Complete consult
  else if consult.status == "Cancelled" then
    NoOp
  else
    NoOp

showConsultation : Address Action -> Consultation -> Html
showConsultation address consult =
  div [ (consultStyle consult) ] [
    span [ class "consultation" ] [
      div [] [ text ("Consultation ID: " ++ (toString consult.id) ++ " / State: " ++ consult.state)],
      div [] [ text ("Status: " ++ consult.status ++ " / Specialty: " ++ consult.specialty)],
      div [] [ text ("Member: " ++ consult.member_name ++ " (ID: " ++ (toString consult.member_id) ++ ")")],
      div [] [ (if (consult.status /= "Completed" && consult.status /= "Cancelled") then (button [onClick address (buttonAction consult)] [ text (buttonText consult) ]) else br [] []) ]
    ]
  ]

ticker : Signal.Signal Int
ticker =
  Signal.foldp (\_ val -> val + 1) 0 (Time.every Time.second)

filteredConsultations model status =
  case model.filterState of
    "" ->
      List.filter (\consult -> consult.status == status) model.consultations
    _ ->
      List.filter (\consult -> consult.state == model.filterState) (List.filter (\consult -> consult.status == status) model.consultations)

--showAllConsultations : Model -> VirtualDom.Node
showAllConsultations address model =
  List.map (showConsultation address) model.consultations

showFilteredConsultations address model status =
  List.map (showConsultation address) (filteredConsultations model status)

teladocImg =
  img [ src "Teladoc_logo.jpg", style [ ("width", "150px"), ("height", "150px") ] ] []

teladocHeadline =
  div [ class "header" ] [
    h1 [] [ text "Super Amazingly Awesome Teladoc Consultation Queue" ]
  ]

classConsultSpan address model class =
  case class of
    "Requested" ->
      requestedConsultSpan address model
    "Locked" ->
      lockedConsultSpan address model
    "Cancelled" ->
      cancelledConsultSpan address model
    "Completed" ->
      completedConsultSpan address model
    _ ->
      completedConsultSpan address model

requestedConsultSpan address model =
  span [ class "consultations", (consultQueueStyle model) ] [
    h2 [] [ text "REQUESTED" ],
    div [ ] (showFilteredConsultations address model "Requested")
  ]

lockedConsultSpan address model =
  span [ class "consultations", (consultQueueStyle model) ] [
    h2 [] [ text "LOCKED" ],
    div [ ] (showFilteredConsultations address model "Locked")
  ]

cancelledConsultSpan address model =
  span [ class "consultations", (consultQueueStyle model) ] [
    h2 [] [ text "CANCELLED" ],
    div [ ] (showFilteredConsultations address model "Cancelled")
  ]

completedConsultSpan address model =
  span [ class "consultations", (consultQueueStyle model) ] [
    h2 [] [ text "COMPLETED" ],
    div [ ] (showFilteredConsultations address model "Completed")
  ]

--teladocOptions address model =
--  div [] [
--    checkbox address model.displayRequested DisplayRequested "Show requested"
--  ]

teladocConsultationQueues address model =
  div [] [
    div [] (List.map (classConsultSpan address model) model.displayedQueues )
  ]

--checkbox : Address Action -> Bool -> (Bool -> Action) -> String -> List Html
checkbox address isChecked tag name =
  div [] [
    input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message address << tag)
      ]
      []
    , text name
  ]

view : Address Action -> Model -> Html
view address model =
  div []
    [ teladocImg
    , teladocHeadline
    , (checkbox address model.displayCancelled DisplayCancelled "Display cancelled?")
    , (checkbox address model.displayCompleted DisplayCompleted "Display completed?")
--    , teladocOptions address model
    , teladocConsultationQueues address model
    ]

modelSignal : Signal Model
modelSignal =
  Signal.foldp update initialModel inbox.signal

main =
  Signal.map (view inbox.address) modelSignal
