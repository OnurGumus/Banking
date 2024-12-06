module Banking.Command.Domain.TransferSaga

open FCQRS
open Akkling
open Akkling.Persistence
open Akka
open Common
open Actor
open Akka.Cluster.Sharding
open Common.SagaStarter
open Akka.Event
open Microsoft.Extensions.Logging
open Akkling.Cluster.Sharding
open Saga
type State =
    | NotStarted
    | Started of SagaStartingEvent<Event<Transfer.Event>>
    | Completed

    interface ISerializable


type SagaData = NA



let initialState = { State = NotStarted; Data = NA }

let apply (sagaState: SagaState<SagaData,State>) =
    match sagaState.State with
    | _ -> sagaState

let handleEvent (event:obj) (state:SagaState<SagaData,State>): option<Effect<_>>  =
    match event, state with
        | :? (Common.Event<Account.Event>) as { EventDetails = subsEvent }, state ->
            match subsEvent, state with
            | _ -> 
                let effect = Completed |> StateChanged |> box |> Persist
                Some effect
        | _ -> None




let applySideEffects env originator (sagaState: SagaState<SagaData,State>) (startingEvent: option<SagaStartingEvent<_>>) recovering =
        match sagaState.State with
        | NotStarted -> NoEffect,Some(Started startingEvent.Value),[] // recovering is always true

        | Started e -> // almost always recovering is false
        //by default recovering should be false here until very exceptional case
            if recovering then // recovering in this case means a crash, will never in practice, but just in case
                // we not issue a continueOrAbort command here, Case 1 or Case 2 will trigger by aggreate
                //subscriptionsActor () <!  continueOrAbort ()
               
          //      let target = Sender
                NoEffect,   None ,[ { TargetActor = originator; Command = Transfer.Continue }]
            else
               Continue, None,[]
        | Completed ->
           Stop, None,[  ]


let init (env: _) toEvent (actorApi: IActor) =
    let originaor =  FactoryAndName { Factory = Transfer.Actor.factory env toEvent actorApi; Name =Originator}
    Saga.init (env: _) actorApi initialState  handleEvent  (applySideEffects env originaor) apply "TransferSaga"

let factory (env: _) toEvent actorApi entityId =
    (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId