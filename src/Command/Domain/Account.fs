module Banking.Command.Domain.Account

open Banking.Model.Data
open FCQRS.Common
open FCQRS

type OperationDetails = {
    UserIdentity: UserIdentity
    AccountName: AccountName
    Money: Money
}
type TransferDetails= {
    OperationDetails: OperationDetails
    DestinationAccountName: AccountName
}
type Event =
    | DepositSuccess of OperationDetails
    | DepositFailed of OperationDetails
    | WithdrawSuccess of OperationDetails
    | WithdrawFailed of OperationDetails
    | TransferSuccess of TransferDetails
    | TransferFailed of TransferDetails

type Command =
    | Deposit of OperationDetails
    | Withdraw of OperationDetails
    | Transfer of TransferDetails



type State = {
    Version: int64
    UserIdentity: UserIdentity option
    AccountName: AccountName option
    Balance: Money option
} with
    interface ISerializable

module internal Actor =
    open Akkling
    open Akka.Cluster.Tools.PublishSubscribe
    open Akkling.Persistence
    open Actor
    open Akkling
    open Microsoft.Extensions.Configuration
    open Microsoft.Extensions.Logging
    open Akka

    type Id = string option
    type CorId = string
    type Version = int64
    open FCQRS.Model.Data

    type ToEvent<'Event> = Id -> CorId -> int64 -> 'Event -> Event<'Event>

    let actorProp (env: _) (toEvent: ToEvent<Event>) (mediator: IActorRef<Publish>) (mailbox: Eventsourced<obj>) =
        let config = env :> IConfiguration
        let loggerFactory = env :> ILoggerFactory
        let logger = loggerFactory.CreateLogger("AccountingActor")

        let apply (event: Event<_>) (_: State as state) =
            match event.EventDetails, state with
            | DepositSuccess { Money = Value money }, { Balance = Some (Value balance) } ->
                let total = balance + money |> ValueLens.Create
                { state with Balance = Some (total) } 
            | DepositFailed  _,  _ -> state
           
            |_ -> state
            |> fun state -> { state with Version = event.Version }

        let rec set (state: State) =
            let body (bodyInput: BodyInput<Event>) =
                let msg = bodyInput.Message
                let version = state.Version
                actor {
                    match msg, state with
                    | :? Persistence.RecoveryCompleted, _ -> 
                        return! state |> set
                    | :? (Common.Command<Command>) as msg, _ ->
                        let toEvent = toEvent (msg.Id) msg.CorrelationId


                        match msg.CommandDetails, state with
                        | Deposit ({ Money = Value  money; UserIdentity = userIdentity  } as details), _ ->
                            if (state.UserIdentity.IsSome && state.UserIdentity.Value <> userIdentity) ||  money < 0m  then
                                let event = toEvent (version ) (DepositFailed details)
                                return! event |> bodyInput.SendToSagaStarter |> Persist
                            else
                                let event = toEvent (version ) (DepositSuccess details)
                                return! event |> bodyInput.SendToSagaStarter |> Persist

                        | _ -> return! set state
                    | _ ->
                        bodyInput.Log.LogWarning("Unhandled message: {msg}", msg)
                        return Unhandled
                }

            runActor logger mailbox mediator set state apply body

        set { Version = 0L; UserIdentity = None; AccountName = None; Balance = None }




    let init (env: _) toEvent (actorApi: IActor) =
        AkklingHelpers.entityFactoryFor actorApi.System shardResolver "Accounting"
        <| propsPersist (actorProp env toEvent (typed actorApi.Mediator))
        <| false

    let factory (env: #_) toEvent actorApi entityId =
        (init env toEvent actorApi).RefFor DEFAULT_SHARD entityId