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
    open Actor
    open FCQRS.Model.Data
    
    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with
        | BalanceUpdated ( b:BalanceUpdateDetails), _ ->
            { state with Account = Some b.Account }
        | MoneyReserved _, _ ->
            { state with Resevations = state.Resevations @ [event] }
        
        | AccountMismatch _, _ 
        | OverdraftAttempted _, _
        | _ -> state
        |> fun state -> { state with Version = event.Version }

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
                        (AccountNotFound , state.Version) |> PersistEvent

                    elif  state.Account.Value.Balance < money then
                        (OverdraftAttempted (state.Account.Value, money) , state.Version) |> PersistEvent
                    else
                        (MoneyReserved money , state.Version + 1L) |> PersistEvent
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

                            BalanceOperation = Receive; 
                            Diff = m

                        } , state.Version + 1L) 
                            |> PersistEvent
                | _ -> UnhandledEvent
           
        | Deposit{ Money = (ResultValue money); UserIdentity = userIdentity; AccountName = accountName }, _ ->
            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity)  then

                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity}, state.Version) |> PersistEvent

            else if state.Account.IsNone then
                let newAccount = { AccountName = accountName; Balance = money; Owner = userIdentity }
                (BalanceUpdated { Account = newAccount; BalanceOperation = BalanceOperation.Deposit; Diff = money } , state.Version + 1L) |> PersistEvent
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance + money) }
                (BalanceUpdated { 
                    Account = account; 
                    BalanceOperation = BalanceOperation.Deposit;
                     Diff = money }
                     , state.Version + 1L) |> PersistEvent
        
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
                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} , state.Version ) |> PersistEvent

            else if state.Account.IsNone || state.Account.Value.Balance < totalReserved then
                (OverdraftAttempted (state.Account.Value, money), state.Version) |> PersistEvent
            else
            let account = { state.Account.Value with Balance = (state.Account.Value.Balance - money) }
            (BalanceUpdated { Account = account; BalanceOperation = BalanceOperation.Withdraw; Diff = money } , state.Version + 1L) |> PersistEvent
        

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; Account = None; Resevations = [] }
        Actor.init (env: _) initialState "Accounting" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
