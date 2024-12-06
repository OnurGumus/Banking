module Banking.Command.Domain.Saga

open FCQRS
open Akkling
open Akkling.Persistence
open Akka
open Common
open Actor
open Akka.Cluster.Sharding
open Common.SagaStarter
open Akka.Event
open Microsoft.Extensions.Logging
open Akkling.Cluster.Sharding


type SagaState<'SagaData,'State> = { Data: 'SagaData; State: 'State }
type TargetName = Name of string | Originator
type FactoryAndName = { Factory: IActor -> string -> IEntityRef<obj>;Name :TargetName }
type TargetActor=
        | FactoryAndName of FactoryAndName
        | Sender

type ExecuteCommand<'Env,'ToEvent> = { TargetActor: TargetActor; Command : obj}
type Effect = 
    | Continue
    | Stop
    | NoEffect

type NextState = obj option



let actorProp<'Command,'SagaData,'TEvent,'Env,'State> (loggerFactory:ILoggerFactory) initialState handleEvent applySideEffects2  apply (actorApi: IActor)  (mediator: IActorRef<_>) (mailbox: Eventsourced<obj>) =
    let cid = (mailbox.Self.Path.Name |> SagaStarter.toCid)
    let log = mailbox.UntypedContext.GetLogger()
    let logger = loggerFactory.CreateLogger("SubscriptionsSaga")
    let createCommand command = {
        CommandDetails = command
        CreationDate = mailbox.System.Scheduler.Now.UtcDateTime
        CorrelationId = cid
        Id = None
    }


    let applySideEffects  (sagaState: SagaState<'SagaData,'State>) (startingEvent: option<SagaStartingEvent<'TEvent>>) recovering =
            let effect, newState, (cmds:ExecuteCommand<_,_> list) = applySideEffects2 sagaState startingEvent recovering
            for cmd in cmds do
                let c= createCommand cmd.Command
                let toEvent ci = Common.toEvent mailbox.System.Scheduler ci
                let targetActor : ICanTell<_> 
                    = match cmd.TargetActor with
                        | FactoryAndName { Factory = factory; Name = n } -> 
                            let name = 
                                match n with
                                | Name n -> n
                                | Originator -> cid |> toOriginatorName
                            factory  actorApi name
                    
                        | Sender -> mailbox.Sender() 
                targetActor <! c
                
            match effect with
            | NoEffect -> 
                newState
            | Continue -> 
                SagaStarter.cont mediator; 
                newState
            | Stop -> 
                mailbox.Parent() <! Passivate(Actor.PoisonPill.Instance)
                log.Info("SubscriptionsSaga Completed");
                 newState
                
          
    let rec set   (sagaState: SagaState<'SagaData,'State>) =

        let body (msg: obj) =
            actor {
                match msg, sagaState with
                | :? (Common.Event<Account.Event>) as { EventDetails = subsEvent }, state ->
                    let state = handleEvent subsEvent state
                    match state with
                    | Some newState ->
                        return! newState
                    | None -> return! sagaState |> set
                | e ->
                    log.Warning("Unhandled event in global {@Event}", e)
                    return Unhandled
            }
        let wrapper = fun (s:'State) -> { Data = sagaState.Data; State = s }
        runSaga mailbox logger mediator set  sagaState applySideEffects apply wrapper body

    set initialState

let init (env: _) (actorApi: IActor) initialState handleEvent applySideEffects apply name=
    (AkklingHelpers.entityFactoryFor actorApi.System shardResolver name
        <| propsPersist (actorProp env  initialState handleEvent applySideEffects  apply  actorApi (typed actorApi.Mediator))
        <| true)
