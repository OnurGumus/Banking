module Banking.Command.API

open FCQRS
open FCQRS.Model.Data
open FCQRS.Actor
open Banking.Model.Command.Accounting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging

[<Interface>]
type IAPI =
    abstract Withdraw: CID -> Withdraw
    abstract Deposit: CID -> Deposit
    abstract Transfer: CID -> Transfer
    abstract ActorApi: IActor

let api (env: _) =
    let config = env :> IConfiguration
    let loggerFactory = env :> ILoggerFactory
    let actorApi = FCQRS.Actor.api config loggerFactory
    // let domainApi = Command.Domain.API.api env actorApi
    // let computeMd5Hash = computeMd5Hash secretSalt

    // let userSubs cid =
    //     createCommandSubscription actorApi domainApi.UserFactory cid
    
    // let subSubs cid =
    //     createCommandSubscription actorApi domainApi.SubscriptionFactory cid


    { new IAPI with

        member _.ActorApi = actorApi        
        member this.Deposit(arg1: CID): Deposit = 
            failwith "Not Implemented"
        member this.Transfer(arg1: CID): Transfer = 
            failwith "Not Implemented"
        member this.Withdraw(arg1: CID): Withdraw = 
            failwith "Not Implemented"

    }