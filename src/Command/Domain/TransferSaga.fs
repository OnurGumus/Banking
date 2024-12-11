module Banking.Command.Domain.TransferSaga

open FCQRS
open Common
open Common.SagaStarter
open Transfer
open FCQRS.Model.Data
open Banking.Model.Data
open Akkling


type TransactionFinalState = Completed | Failed

type State =
    | NotStarted
    | Started of SagaStartingEvent<Event<Transfer.Event>>
    | TransferStarted of TransferEventDetails
    | ReservingSender
    | ReservingReceiver
    | ConfirmingSender
    | ConfirmingReceiver
    | CompletingTransfer of TransactionFinalState
    | Completed

    interface ISerializable

type SagaData = { TransferEventDetails: TransferEventDetails option; }

let initialState = { State = NotStarted; Data = {TransferEventDetails = None }  }

let apply (sagaState: SagaState<SagaData,State>) =
    match sagaState.State with
    | TransferStarted e -> 
        { sagaState with Data = { TransferEventDetails = Some e} }
    | _ -> sagaState

let handleEvent (event:obj) (state:SagaState<SagaData,State>)= //: EventAction<State>  =
    match event, state with
        | :? (Common.Event<Transfer.Event>) as { EventDetails = accountEvent }, state ->
            match accountEvent, state with
            | Transfer.TransferRequested e,  _ -> TransferStarted e |>  StateChangedEvent
            | Transfer.MoneyTransferred _ ,  _ 
            | Transfer.TransferAborted,  _ ->
                Completed  |> StateChangedEvent
            | _ ->  UnhandledEvent
            
        | :? (Common.Event<Account.Event>) as { EventDetails = accountEvent }, state ->
            match accountEvent, state.State with
            | Account.AccountNotFound,  _   -> 
                CompletingTransfer TransactionFinalState.Failed  |> StateChangedEvent
            | Account.OverdraftAttempted _,  State.ReservingSender   -> 
                CompletingTransfer TransactionFinalState.Failed  |> StateChangedEvent
            | Account.MoneyReserved _,  State.ReservingSender   -> ReservingReceiver  |> StateChangedEvent
            | Account.MoneyReserved _,  State.ReservingReceiver   -> ConfirmingSender  |> StateChangedEvent

            | Account.NoReservationFound,  State.ConfirmingSender 
            | Account.BalanceUpdated _,  State.ConfirmingSender   -> 
                ConfirmingReceiver  |> StateChangedEvent

            | Account.NoReservationFound,  State.ConfirmingSender 
            | Account.BalanceUpdated _,  State.ConfirmingReceiver   -> 
                CompletingTransfer TransactionFinalState.Completed  |> StateChangedEvent
    
            | _ ->  Completed   |> StateChangedEvent
        | _ -> UnhandledEvent


let applySideEffects (actorRef:ICanTell<obj>) env transferFactory accountFactory  (sagaState:SagaState<SagaData,State>) (startingEvent: option<SagaStartingEvent<_>>) recovering =
    
    let accountActor (accountName:AccountName) = 
        let accountName  =  
            accountName
            |> ValueLens.Value 
            |> ValueLens.Value
        let actorId  = "Account_" +  accountName
        FactoryAndName { Factory = accountFactory; Name = Name actorId}    
        
    match sagaState.State with
        | NotStarted -> NoEffect,Some(Started startingEvent.Value),[] // recovering is always true

        | Started _ -> // almost always recovering is false
                //by default recovering should be false here until very exceptional case
            if recovering then // recovering in this case means a crash, will never in practice, but just in case
                // we not issue a continueOrAbort command here, Case 1 or Case 2 will trigger by aggreate
                let originator = FactoryAndName { Factory = transferFactory; Name = Originator}
                NoEffect,   None ,[ { TargetActor = originator; Command = Transfer.Continue;  }]
            else
               ResumeFirstEvent, None,[]
// { TargetActor = ActorRef actorRef; Command = "Transfer.Continue";  }
        | TransferStarted e ->
           NoEffect, Some ReservingSender , [ { TargetActor = ActorRef actorRef; Command = "Onur";  }]

        | ReservingSender ->
            let target = accountActor sagaState.Data.TransferEventDetails.Value.From
            let money = sagaState.Data.TransferEventDetails.Value.Amount |> ValueLens.Value
            
            NoEffect, None ,[{ TargetActor = target; Command = Account.ReserveMoney money  }]

        | ReservingReceiver ->

            let target = accountActor sagaState.Data.TransferEventDetails.Value.To
            let money = sagaState.Data.TransferEventDetails.Value.Amount |> ValueLens.Value |> Money.Negate

            NoEffect, None ,[{ TargetActor = target; Command = Account.ReserveMoney money  }]

        | ConfirmingReceiver ->  
                let target = accountActor sagaState.Data.TransferEventDetails.Value.To
                NoEffect, None ,[{ TargetActor = target; Command = Account.ConfirmReservation   }]

        | ConfirmingSender ->  
                let target = accountActor sagaState.Data.TransferEventDetails.Value.From
                NoEffect, None ,[{ TargetActor = target; Command = Account.ConfirmReservation   }]
        | CompletingTransfer Failed->  
           
            NoEffect, None,[{ TargetActor =  FactoryAndName { Factory = transferFactory; Name = Originator}  ; Command = Transfer.MarkTransferCompleted Status.Failed  }]
        | CompletingTransfer TransactionFinalState.Completed ->
            NoEffect, None,[{ TargetActor =  FactoryAndName { Factory = transferFactory; Name = Originator}  ; Command = Transfer.MarkTransferCompleted Status.Completed  }]

        | Completed ->
           StopActor, None,[  ]


let  init (env: _)  (actorApi: IActor) =
    let transferFactory =  Transfer.Actor.factory env  actorApi
    let accountFactory =  Account.Actor.factory env  actorApi
    let behavior (m: Actor<_>) =
        let rec loop () =
            actor {
                let! msg = m.Receive()
    
                match msg with
                | "stop" -> return Stop
                | "unhandle" -> return Unhandled
                | x ->
                    printfn "!!!%s %A" x (m.Sender().Path)
                    return! loop ()
            }
    
        loop ()
    let helloRef: IActorRef<obj> = spawnAnonymous actorApi.System (props behavior) |> retype

    actorApi.InitializeSaga env initialState  handleEvent (applySideEffects (helloRef) env transferFactory accountFactory) apply "TransferSaga"

let  factory (env: _)  actorApi entityId =
    (init env  actorApi).RefFor DEFAULT_SHARD entityId