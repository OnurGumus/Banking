module Banking.Command.Domain.Account

open Banking.Model.Data
open FCQRS.Common
open FCQRS

type BalanceUpdateDetails ={ Account : Account ; Diff : Money}
type AccountMismatch = { TargetAccount : Account; TargetUser : UserIdentity }
type Event =
    | BalanceUpdated of BalanceUpdateDetails
    | OverdraftAttempted of Account * Money
    | AccountMismatch of  AccountMismatch
    | AccountNotFound
    | NoReservationFound
    | MoneyReserved of Money
    
type Command =
    | Deposit of OperationDetails
    | Withdraw of OperationDetails
    | ReserveMoney of Money
    | ConfirmReservation

type State = {
    Account: Account option
    Resevations: Event<Event> list
} with

    interface ISerializable

module internal Actor =
    open Actor
    open FCQRS.Model.Data
    
    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with
        | BalanceUpdated ( b:BalanceUpdateDetails), _ ->
            let state  ={ state with Account = Some b.Account }
            if state.Resevations |> List.exists (fun x -> x.CorrelationId = event.CorrelationId) then
                { state 
                    with Resevations = state.Resevations 
                        |> List.filter (fun x -> x.CorrelationId <> event.CorrelationId) }
            else state

        | MoneyReserved _, _ ->
            { state with Resevations = state.Resevations @ [event] }
        
        | AccountMismatch _, _ 
        | OverdraftAttempted _, _
        | _ -> state

    let handleCommand (cmd:Command<_>) (state:State)  =
        let corID = cmd.CorrelationId
        match cmd.CommandDetails, state with

        | ReserveMoney money, _ ->
            let existingEvent =  
                state.Resevations |> List.tryFind (fun x -> x.CorrelationId = corID) 
            let eventAcion : EventAction<Event> =
                match existingEvent with
                | Some x -> x |> PublishEvent
                | None -> 
                    if state.Account.IsNone  
                    then
                        AccountNotFound |> PersistEvent

                    elif  state.Account.Value.Balance < money then
                        (OverdraftAttempted (state.Account.Value, money)) |> PersistEvent
                    else
                        (MoneyReserved money ) |> PersistEvent
            eventAcion

        | ConfirmReservation, _ ->
           let findReservation = state.Resevations |> List.tryFind (fun x -> x.CorrelationId = corID) |> Option.map (fun x -> x.EventDetails) 
           match findReservation with
            
                | Some (MoneyReserved(m)) ->
                    (BalanceUpdated 
                        { 
                            Account = 
                                { state.Account.Value 
                                    with Balance = state.Account.Value.Balance - (m |> ValueLens.Value) }; 

                            Diff = m

                        } ) 
                            |> PersistEvent
                | _ -> (NoReservationFound) |> DeferEvent
           
        | Deposit{ Money = (ResultValue money); UserIdentity = userIdentity; AccountName = accountName }, _ ->
            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity)  then

                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity}) |> DeferEvent

            else if state.Account.IsNone then
                let newAccount = { AccountName = accountName; Balance = money; Owner = userIdentity }
                (BalanceUpdated { Account = newAccount; Diff = money } ) |> PersistEvent
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance + money) }
                (BalanceUpdated { 
                    Account = account; 
                     Diff = money }) |> PersistEvent
        
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
                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} ) |> DeferEvent

            elif state.Account.IsNone  then
               AccountNotFound |> DeferEvent
            elif state.Account.Value.Balance < totalReserved then
                (OverdraftAttempted (state.Account.Value, money)) |> DeferEvent
            else
            let account = { state.Account.Value with Balance = (state.Account.Value.Balance - money) }
            (BalanceUpdated { Account = account; Diff = money } ) |> PersistEvent
        

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = {  Account = None; Resevations = [] }
        Actor.init (env: _) initialState "Accounting" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
