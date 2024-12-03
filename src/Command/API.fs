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
    let domainApi = Command.Domain.API.api env actorApi
    let accountSubs cid =  createCommandSubscription actorApi domainApi.AccountFactory cid


    { new IAPI with

        member _.ActorApi = actorApi        
        member this.Deposit((Value (ResultValue cid)): CID): Deposit = 
            AccountingHandler.deposit (accountSubs  cid)
        member this.Transfer(arg1: CID): Transfer = 
            failwith "Not Implemented"
        member this.Withdraw(arg1: CID): Withdraw = 
            failwith "Not Implemented"

    }