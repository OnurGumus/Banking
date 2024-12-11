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

    let handleCommand (cmd:Command<_>) (state:State)  =
        match cmd.CommandDetails, state with
        | Transfer _, { TransferDetails = Some _ } ->
            AnotherTransferIsInProgress |> DeferEvent

        | Transfer transferDetails, { TransferDetails = None } ->
            (TransferRequested { From = transferDetails.OperationDetails.AccountName; 
                To = transferDetails.DestinationAccountName; Amount = transferDetails.OperationDetails.Money })  |> PersistEvent

        // Not going to happen in practice, but we need to handle it
        | Continue, { LastEvents = {TransferRequestedEvent = Some event}} ->
            event |> PublishEvent  
        
         // Not going to happen in practice, but we need to handle it            
        | Continue, {LastEvents =  { TransferRequestedEvent = None } }->
            TransferAborted  |> DeferEvent

        | MarkTransferCompleted Status.Completed, { LastEvents =  { MoneyTransferredEvent = None} } ->
            (MoneyTransferred { 
                From = state.TransferDetails.Value.From
                To = state.TransferDetails.Value.To; 
                Amount = state.TransferDetails.Value.Amount } ) |> PersistEvent

        | MarkTransferCompleted Status.Completed, { LastEvents =  { MoneyTransferredEvent = Some e} } ->
            e.EventDetails  |> PersistEvent
            
        | MarkTransferCompleted Status.Failed, _ ->
            TransferAborted|> PersistEvent
         

    let init (env: _) (actorApi: IActor) =
        let initialState = { TransferDetails = None; LastEvents = { TransferRequestedEvent = None; MoneyTransferredEvent = None } }
        actorApi.InitializeActor env initialState "Transfer"  handleCommand applyEvent

    let factory (env: #_)  actorApi entityId =
        (init env  actorApi).RefFor DEFAULT_SHARD entityId
