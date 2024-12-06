module Banking.Command.Domain.TransferSaga

open FCQRS
open Akkling
open Akka
open Common
open Actor
open Common.SagaStarter
open Saga
open Transfer
open FCQRS.Model.Data
open Banking.Model.Data

type State =
    | NotStarted
    | Started of SagaStartingEvent<Event<Transfer.Event>>
    | TransferStarted of TransferEventDetails
    | ReservingSender
    | ReservingReceiver
    | ConfirmingSender
    | ConfirmingReceiver
    | CompletingSenderAndReceiver
    | CompletingTransfer
    | Completed

    interface ISerializable

type SagaData = { TransferEventDetails: TransferEventDetails option; }


let initialState = { State = NotStarted; Data = {TransferEventDetails = None }  }

let apply (sagaState: SagaState<SagaData,State>) =
    match sagaState.State with
    | TransferStarted e -> 
        { sagaState with Data = { TransferEventDetails = Some e} }
    | _ -> sagaState

let handleEvent (event:obj) (state:SagaState<SagaData,State>): option<Effect<_>>  =
    match event, state with
        | :? (Common.Event<Transfer.Event>) as { EventDetails = accountEvent }, state ->
            match accountEvent, state with
            | Transfer.TransferRequested e,  _ -> TransferStarted e |> toStateChange
        | :? (Common.Event<Account.Event>) as { EventDetails = accountEvent }, state ->
            match accountEvent, state.State with
            | Account.MoneyReserved e,  State.ReservingSender   -> ReservingReceiver  |> toStateChange
            | Account.MoneyReserved e,  State.ReservingReceiver   -> ConfirmingReceiver  |> toStateChange
            | Account.BalanceUpdated e,  State.ConfirmingReceiver   -> ConfirmingSender  |> toStateChange
            | Account.BalanceUpdated e,  State.ConfirmingSender   -> CompletingSenderAndReceiver  |> toStateChange
    
            | _ ->  Completed   |> toStateChange
        | _ -> None


let applySideEffects env transferFactory accountFactory  (sagaState:SagaState<SagaData,State>) (startingEvent: option<SagaStartingEvent<_>>) recovering =
    
    let accountActor (accountName:AccountName) = 
        let accountName  =  
            accountName
            |> ValueLens.Value 
            |> ValueLens.Value
        let actorId  = "Account_" +  accountName
        FactoryAndName { Factory = accountFactory; Name = Name actorId}    
        
    match sagaState.State with
        | NotStarted -> NoEffect,Some(Started startingEvent.Value),[] // recovering is always true

        | Started e -> // almost always recovering is false
                //by default recovering should be false here until very exceptional case
            if recovering then // recovering in this case means a crash, will never in practice, but just in case
                // we not issue a continueOrAbort command here, Case 1 or Case 2 will trigger by aggreate
                let originator = FactoryAndName { Factory = transferFactory; Name = Originator}
                NoEffect,   None ,[ { TargetActor = originator; Command = Transfer.Continue;  }]
            else
               ResumeFirstEvent, None,[]

        | TransferStarted e ->
           NoEffect, Some ReservingSender ,[]

        | ReservingSender ->
            let target = accountActor sagaState.Data.TransferEventDetails.Value.From
            let money = sagaState.Data.TransferEventDetails.Value.Amount |> ValueLens.Value
            
            NoEffect, None ,[{ TargetActor = target; Command = Account.ReserveMoney money  }]

        | ReservingReceiver ->

        let target = accountActor sagaState.Data.TransferEventDetails.Value.To
        let money = sagaState.Data.TransferEventDetails.Value.Amount |> ValueLens.Value |> Money.Negate

        NoEffect, None ,[{ TargetActor = target; Command = Account.ReserveMoney money  }]

        | ConfirmingReceiver ->  NoEffect, None,[]
        | ConfirmingSender ->  NoEffect, None,[]
        | CompletingSenderAndReceiver ->  NoEffect, None,[]
        | CompletingTransfer ->  NoEffect, None,[]
        | Completed ->
           Stop, None,[  ]


let  init (env: _)  (actorApi: IActor) =
    let toEvent v e =
        Common.toEvent actorApi.System.Scheduler v e
    let transferFactory =  Transfer.Actor.factory env toEvent actorApi
    let accountFactory =  Account.Actor.factory env toEvent actorApi
    Saga.init (env: _) actorApi initialState  handleEvent  (applySideEffects env transferFactory accountFactory) apply "TransferSaga"

let  factory (env: _)  actorApi entityId =
    (init env  actorApi).RefFor DEFAULT_SHARD entityId