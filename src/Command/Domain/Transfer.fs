module Banking.Command.Domain.Transfer

open Banking.Model.Data
open FCQRS.Common
open FCQRS
type TransferEventDetails = { From: AccountName; To: AccountName; Amount: PositiveMoney }

type Status = Completed | Failed
type Event =
    | MoneyTransferred of TransferEventDetails
    | TransferRequested of TransferEventDetails
    | TransferAborted
    | AnotherTransferIsInProgress
    
type Command =
    | Transfer of TransferDetails
    | MarkTransferCompleted of Status
    | Continue

type LastEvents = {  
        TransferRequestedEvent: Event<Event> option; 
        MoneyTransferredEvent:Event<Event> option 
    }

type State = {
    Version: int64
    TransferDetails: TransferEventDetails option
    LastEvents: LastEvents 
} with

    interface ISerializable

module internal Actor =
    open Actor


    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with
        | TransferRequested e, _ ->
            { state with TransferDetails = Some e ; LastEvents = { state.LastEvents with TransferRequestedEvent = Some event } }
        | _ -> state
        |> fun state -> { state with Version = event.Version }

    let handleCommand (cmd:Command<_>) (state:State)  =
        match cmd.CommandDetails, state with
        | Transfer _, { TransferDetails = Some _ } ->
            (AnotherTransferIsInProgress  , state.Version) |> DeferEvent

        | Transfer transferDetails, { TransferDetails = None } ->
            (TransferRequested { From = transferDetails.OperationDetails.AccountName; To = transferDetails.DestinationAccountName; Amount = transferDetails.OperationDetails.Money } 
                 , state.Version + 1L) |> PersistEvent

        // Not going to happen in practice, but we need to handle it
        | Continue, { LastEvents = {TransferRequestedEvent = Some event}} ->
            event |> PublishEvent  
        
         // Not going to happen in practice, but we need to handle it            
        | Continue, {LastEvents =  { TransferRequestedEvent = None } }->
            (TransferAborted  , state.Version) |> DeferEvent

        | MarkTransferCompleted Status.Completed, { LastEvents =  { MoneyTransferredEvent = None} } ->
            (MoneyTransferred { 
                From = state.TransferDetails.Value.From
                To = state.TransferDetails.Value.To; 
                Amount = state.TransferDetails.Value.Amount } 
                 , state.Version + 1L) |> PersistEvent

        | MarkTransferCompleted Status.Completed, { LastEvents =  { MoneyTransferredEvent = Some e} } ->
            (e.EventDetails , state.Version + 1L) |> PersistEvent
            
        | MarkTransferCompleted Status.Failed, _ ->
            (TransferAborted , state.Version + 1L) |> PersistEvent
         

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; TransferDetails = None; LastEvents = { TransferRequestedEvent = None; MoneyTransferredEvent = None } }
        Actor.init (env: _) initialState "Transaction" (toEvent) (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
