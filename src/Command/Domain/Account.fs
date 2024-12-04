module Banking.Command.Domain.Account

open Banking.Model.Data
open FCQRS.Common
open FCQRS



type Event =
    | DepositSuccess of OperationDetails
    | DepositFailed of OperationDetails
    | WithdrawSuccess of OperationDetails
    | WithdrawFailed of OperationDetails

type Command =
    | Deposit of OperationDetails
    | Withdraw of OperationDetails

type State = {
    Version: int64
    UserIdentity: UserIdentity option
    AccountName: AccountName option
    Balance: Money option
} with

    interface ISerializable

module internal Actor =
    open Akkling.Persistence
    open Actor
    open FCQRS.Model.Data


    let applyEvent (event: Event<_>) (_: State as state) =
        match event.EventDetails, state with

        | DepositSuccess { Money = Value money }, { Balance = Some(Value balance) } ->
            let total = balance + money |> ValueLens.Create
            { state with Balance = Some(total) }

        | WithdrawSuccess { Money = Value money }, { Balance = Some(Value balance) } ->
            let total = balance - money |> ValueLens.Create
            { state with Balance = Some(total) }
            
        | WithdrawFailed _, _
        | DepositFailed _, _ -> state

        | _ -> state
        |> fun state -> { state with Version = event.Version }

    let handleCommand cmd state  =
        match cmd, state with
        
        | Deposit({ Money = Value money; UserIdentity = userIdentity } as details), _ ->
            if (state.UserIdentity.IsSome && state.UserIdentity.Value <> userIdentity) || money < 0m then
                Some(Persist(DepositFailed details), state.Version)
            else
                Some(Persist(DepositSuccess details), state.Version)

        | Withdraw({ Money = Value money; UserIdentity = userIdentity } as details), _ ->
            if
                (state.UserIdentity.IsSome && state.UserIdentity.Value <> userIdentity)
                || money < 0m
                || state.Balance.IsNone
                || (state.Balance.Value |> ValueLens.Value) < money
            then
                Some(Persist(WithdrawFailed details), state.Version)
            else
                Some(Persist(WithdrawSuccess details), state.Version)



    let init (env: _) toEvent (actorApi: IActor) =
        let initialState = { Version = 0L; UserIdentity = None; AccountName = None; Balance = None }
        Actor.init (env: _) initialState "Accounting" toEvent (actorApi: IActor) handleCommand applyEvent

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId
