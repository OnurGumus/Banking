module Banking.Command.Domain.Account

open Banking.Model.Data
open FCQRS.Common
open FCQRS

type BalanceOperation = Deposit | Withdraw
type BalanceUpdateDetails ={ Account : Account; BalanceOperation : BalanceOperation ; Diff : Money}
type AccountMismatch = { TargetAccount : Account; TargetUser : UserIdentity }
type Event =
    | BalanceUpdated of BalanceUpdateDetails
    | OverdraftAttempted of Account * Money
    | AccountMismatch of  AccountMismatch
    
type Command =
    | Deposit of OperationDetails
    | Withdraw of OperationDetails

type State = {
    Version: int64
    Account: Account option
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

        | OverdraftAttempted _, _
        | _ -> state
        |> fun state -> { state with Version = event.Version }

    let handleCommand cmd (state:State)  =
        match cmd, state with
        
        | Deposit{ Money = (ResultValue money); UserIdentity = userIdentity; AccountName = accountName }, _ ->
            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity)  then
                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} |> Persist, state.Version) |> Some
            else if state.Account.IsNone then
                let newAccount = { AccountName = accountName; Balance = money; Owner = userIdentity }
                (BalanceUpdated { Account = newAccount; BalanceOperation = BalanceOperation.Deposit; Diff = money } |> Persist, state.Version + 1L) |> Some
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance + money) }
                (BalanceUpdated { Account = account; BalanceOperation = BalanceOperation.Deposit; Diff = money } |> Persist, state.Version + 1L) |> Some
        
        | Withdraw{ Money = (ResultValue money); UserIdentity = userIdentity }, _ ->
            if (state.Account.IsSome && state.Account.Value.Owner <> userIdentity) then
                (AccountMismatch { TargetAccount = state.Account.Value; TargetUser = userIdentity} |> Persist, state.Version ) |> Some
            else if state.Account.IsNone || state.Account.Value.Balance < money then
                (OverdraftAttempted (state.Account.Value, money) |> Persist, state.Version) |> Some
            else
                let account = { state.Account.Value with Balance = (state.Account.Value.Balance - money) }
                (BalanceUpdated { Account = account; BalanceOperation = BalanceOperation.Withdraw; Diff = money } |> Persist, state.Version + 1L) |> Some
            

    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; Account = None}
        Actor.init (env: _) initialState "Accounting" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
