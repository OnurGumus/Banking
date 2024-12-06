module internal Command.Domain.API

open FCQRS
open Akkling
open Akka
open Common
open Actor
open Akkling.Cluster.Sharding
open Banking.Command.Domain

let sagaCheck (env: _) toEvent actorApi (o: obj) =
    printfn "%A" o
    match o with
    | :? (Event<Transfer.Event>) as e ->
            match e.EventDetails with
            | Transfer.TransferRequested _ ->
            [
                (TransferSaga.factory env toEvent actorApi, id |> Some |> PrefixConversion, o)
             ]
             | _ -> []
        | _ -> []
    | _ -> []
[<Interface>]

type IDomain =
    abstract ActorApi: IActor
    abstract AccountFactory: string -> IEntityRef<obj>
    abstract TransferFactory: string -> IEntityRef<obj>

let api (env: #_) (actorApi: IActor) =
        let  toEvent v e =
            Common.toEvent actorApi.System.Scheduler v e

        let scr = (sagaCheck env toEvent actorApi)
        
        SagaStarter.init actorApi.System actorApi.Mediator scr
        Account.Actor.init env toEvent actorApi |> ignore
        Transfer.Actor.init env toEvent actorApi |> ignore
        TransferSaga.init env toEvent actorApi |> ignore

        System.Threading.Thread.Sleep(1000)

        { new IDomain with
            member _.ActorApi = actorApi
            member _.TransferFactory entityId =
                Transfer.Actor.factory env toEvent actorApi entityId
            member _.AccountFactory entityId =
                Account.Actor.factory env toEvent actorApi entityId
        }
    