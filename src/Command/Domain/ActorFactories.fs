module internal Command.Domain.ActorFactories

open FCQRS
open Akkling
open Akka
open Common
open Actor
open Akkling.Cluster.Sharding
open Banking.Command.Domain

[<Interface>]
type IActorFactories =
    abstract AccountFactory: string -> IEntityRef<obj>
    abstract TransferFactory: string -> IEntityRef<obj>

let factories (env: #_) (actorApi: IActor) =
        let  toEvent v e =
            Common.toEvent actorApi.System.Scheduler v e

        let transferShard =  Transfer.Actor.factory env  actorApi
        let accountShard =  Account.Actor.factory env actorApi
        let tranferSagaShard = TransferSaga.factory env actorApi

        let sagaCheck  (o: obj) =
            match o with
            | :? (Event<Transfer.Event>) as e ->
                    match e.EventDetails with
                    | Transfer.TransferRequested _ ->
                        [ tranferSagaShard , id |> Some |> PrefixConversion, o]
                        | _ -> []
                | _ -> []

        actorApi.InitializeSagStarter sagaCheck
        Account.Actor.init env  actorApi |> ignore
        Transfer.Actor.init env  actorApi |> ignore
        TransferSaga.init env actorApi |> ignore


        { new IActorFactories with
            member _.TransferFactory entityId =
                transferShard entityId
            member _.AccountFactory entityId =
                accountShard entityId
        }    