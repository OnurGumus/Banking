module Banking.Command.Domain.Account

open Banking.Model.Data
open FCQRS.Common
open FCQRS

type BalanceOperation = Deposit | Withdraw | Send | Receive
type BalanceUpdateDetails ={ Account : Account; BalanceOperation : BalanceOperation ; Diff : Money}
type AccountMismatch = { TargetAccount : Account; TargetUser : UserIdentity }
type Event =
    | BalanceUpdated of BalanceUpdateDetails
    | OverdraftAttempted of Account * Money
    | AccountMismatch of  AccountMismatch
    | AccountNotFound
    | MoneyReserved of Money
    // | TransferCompleted

    
type Command =
    | Deposit of OperationDetails
    | Withdraw of OperationDetails
    | ReserveMoney of Money
    | ConfirmReservation


type State = {
    Version: int64
    Account: Account option
    Resevations: Event<Event> list
} with

    interface ISerializable

module internal Actor =
    open Akkling.Persistence
    open Actor
    open FCQRS.Model.Data

    
    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with
        | BalanceUpdated ( b:BalanceUpdateDetails), _ ->
            { state with Account = Some b.Account }
        | MoneyReserved _, _ ->
            { state with Resevations = state.Resevations @ [event] }
        // | TransferCompleted, _ -> 
        //     { state with Resevations = state.Resevations |> List.filter (fun x -> x.CorrelationId <> event.CorrelationId) }
        
        | AccountMismatch _, _ 
        | OverdraftAttempted _, _
        | _ -> state
        |> fun state -> { state with Version = event.Version }

    let handleCommand (cmd:Command<_>) (state:State)  =
        let corID = cmd.CorrelationId
        match cmd.CommandDetails, state with
        // | CompleteTransfer , _ ->
        //     (TransferCompleted |> Persist, state.Version) |> Some

        | ReserveMoney money, _ ->
            let existingEvent =  
                state.Resevations |> List.tryFind (fun x -> x.CorrelationId = corID) 
                |> Option.bind (fun x -> Some (seq{MoneyReserved money} |> Defer, state.Version))

            match existingEvent with
            | Some x -> x |> Some
            | None -> 
                if state.Account.IsNone  
                then
                    (AccountNotFound |> Persist, state.Version) |> Some

                elif  state.Account.Value.Balance < money then
                    (OverdraftAttempted (state.Account.Value, money) |> Persist, state.Version) |> Some
                else
                    (MoneyReserved money |> Persist, state.Version + 1L) |> Some

        | ConfirmReservation, _ ->
           let findReservation = state.Resevations |> List.tryFind (fun x -> x.CorrelationId = corID) |> Option.map (fun x -> x.EventDetails) 
           match findReservation with
            
                | Some (MoneyReserved(m)) ->
                    (BalanceUpdated 
                        { 
                            Account = 
                                { state.Account.Value 
                                    with Balance = state.Account.Value.Balance - (m |> ValueLens.Value) }; 

                            BalanceOperation = Receive; 
                            Diff = m

                        } |> Persist, state.Version + 1L) 
                            |> Some
                | _ -> None
           
        | Deposit{ Money = (ResultValue money); UserIdentity = userIdentity; AccountName = accountName }, _ ->
            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity)  then

                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} 
                    |> Persist, state.Version) |> Some

            else if state.Account.IsNone then
                let newAccount = { AccountName = accountName; Balance = money; Owner = userIdentity }
                (BalanceUpdated { Account = newAccount; BalanceOperation = BalanceOperation.Deposit; Diff = money } 
                    |> Persist, state.Version + 1L) |> Some
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance + money) }
                (BalanceUpdated { 
                    Account = account; 
                    BalanceOperation = BalanceOperation.Deposit;
                     Diff = money }
                     |> Persist, state.Version + 1L) |> Some
        
        | Withdraw{ Money = (ResultValue money); UserIdentity = userIdentity }, _ ->
            let totalReserved = 
                    state.Resevations
                    |> List.map (fun eventWrapper -> eventWrapper.EventDetails)
                    |> List.sumBy (fun event -> 
                        match event  with 
                        | MoneyReserved m when m > Money.Zero -> m  
                        | _ -> Money.Zero
                    )

            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity) then
                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} |> Persist, state.Version ) |> Some

            else if state.Account.IsNone || state.Account.Value.Balance < totalReserved then
                (OverdraftAttempted (state.Account.Value, money) |> Persist, state.Version) |> Some
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance - money) }
                (BalanceUpdated { Account = account; BalanceOperation = BalanceOperation.Withdraw; Diff = money } |> Persist, state.Version + 1L) |> Some
            

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; Account = None; Resevations = [] }
        Actor.init (env: _) initialState "Accounting" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
