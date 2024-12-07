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
    let transferSubs cid =  createCommandSubscription actorApi domainApi.TransferFactory cid


    { new IAPI with
        member _.ActorApi = actorApi        
        member this.Deposit cid: Deposit = 
            AccountingHandler.deposit (accountSubs  cid)
        member this.Transfer cid: Transfer = 
            TransferHandler.transfer (transferSubs  cid)
        member this.Withdraw cid: Withdraw = 
            AccountingHandler.withdraw (accountSubs  cid)

    }