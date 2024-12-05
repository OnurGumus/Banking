module Banking.Command.Domain.Transfer

open Banking.Model.Data
open FCQRS.Common
open FCQRS
type TransferEventDetails = { From: AccountName; To: AccountName; Amount: PositiveMoney }

type Event =
    | MoneyTransferred of TransferEventDetails
    | TransferRequested of TransferEventDetails
    | TransferAborted
    | AnotherTransferIsInProgress
    
type Command =
    | Transfer of TransferDetails
    | MarkTransferCompleted
    | Continue
type LastEvents = {  TransferRequestedEvent: Event option; MoneyTransferredEvent: Event option }

type State = {
    Version: int64
    TransferDetails: TransferDetails option
    LastEvents: LastEvents 
} with

    interface ISerializable

module internal Actor =
    open Akkling.Persistence
    open Actor
    open FCQRS.Model.Data


    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with
        | _ -> state
        |> fun state -> { state with Version = event.Version }

    let handleCommand (cmd:Command<_>) (state:State)  =
        match cmd.CommandDetails, state with
        | Transfer _, { TransferDetails = Some _ } ->
            (AnotherTransferIsInProgress |> Persist, state.Version) |> Some

        | Transfer transferDetails, { TransferDetails = None } ->
            (TransferRequested { From = transferDetails.OperationDetails.AccountName; To = transferDetails.DestinationAccountName; Amount = transferDetails.OperationDetails.Money }
                 |> Persist, state.Version + 1L) |> Some

        | Continue, { LastEvents = {TransferRequestedEvent = Some event}} ->
            ( seq{event} |> Defer, state.Version ) |> Some
            
        | Continue, {LastEvents =  { TransferRequestedEvent = None } }->
            (seq{TransferAborted} |>Defer, state.Version) |> Some

        | MarkTransferCompleted, { LastEvents =  { MoneyTransferredEvent = None} } ->
            (MoneyTransferred { 
                From = state.TransferDetails.Value.OperationDetails.AccountName; 
                To = state.TransferDetails.Value.DestinationAccountName; 
                Amount = state.TransferDetails.Value.OperationDetails.Money }
                 |> Persist, state.Version + 1L) |> Some

        | MarkTransferCompleted, { LastEvents =  { MoneyTransferredEvent = Some e} } ->
            (seq{e} |> Defer, state.Version) |> Some
         

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; TransferDetails = None; LastEvents = { TransferRequestedEvent = None; MoneyTransferredEvent = None } }
        Actor.init (env: _) initialState "Transaction" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
