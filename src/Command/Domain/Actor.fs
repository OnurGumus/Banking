module Banking.Actor


open Banking.Model.Data
open FCQRS.Common
open FCQRS


    open Akkling
    open Akka.Cluster.Tools.PublishSubscribe
    open Akkling.Persistence
    open Actor
    open Akkling
    open Microsoft.Extensions.Configuration
    open Microsoft.Extensions.Logging
    open Akka
    open FCQRS.Model.Data

    type Id = string option
    type CorId = string
    type Version = int64
    type ToEvent<'Event> = Id -> CorId -> int64 -> 'Event -> Event<'Event>

    let  actorProp<'Command,'State,'Event,'Env> (loggerFactory:ILoggerFactory) handleCommand apply  initialState  (name:string) (toEvent: ToEvent<'Event>) (mediator: IActorRef<Publish>) (mailbox: Eventsourced<obj>)  =
        let logger = loggerFactory.CreateLogger(name)
        let rec set (state: 'State) =
            let body (bodyInput: BodyInput<'Event>) =
                let msg = bodyInput.Message

                actor {
                    match msg, state with
                    | :? Persistence.RecoveryCompleted, _ -> return! state |> set
                    | :? (Common.Command<'Command>) as msg, _ ->
                        let toEvent = toEvent (msg.Id) msg.CorrelationId

                        match handleCommand msg.CommandDetails state  with
                        | Some(Persist event, version) ->
                            return! event |> toEvent version |> bodyInput.SendToSagaStarter |> Persist
                        | _ -> return set state
                    | _ ->
                        bodyInput.Log.LogWarning("Unhandled message: {msg}", msg)
                        return Unhandled
                }

            runActor logger mailbox mediator set state apply body
        set  initialState


    let init (env: _) initialState name toEvent (actorApi: IActor) handleCommand apply =
        let loggerFactory = env :> ILoggerFactory
        AkklingHelpers.entityFactoryFor actorApi.System shardResolver name
        <| propsPersist (actorProp loggerFactory handleCommand apply initialState  name  toEvent (typed actorApi.Mediator)) 
        <| false