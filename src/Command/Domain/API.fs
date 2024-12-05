module internal Command.Domain.API

open FCQRS
open Akkling
open Akka
open Common
open Actor
open Akkling.Cluster.Sharding
open Banking.Command.Domain

let sagaCheck (env: _) toEvent actorApi (o: obj) =
    match o with
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

        System.Threading.Thread.Sleep(1000)

        { new IDomain with
            member _.ActorApi = actorApi
            member _.TransferFactory entityId =
                Transfer.Actor.factory env toEvent actorApi entityId
            member _.AccountFactory entityId =
                Account.Actor.factory env toEvent actorApi entityId
        }
    