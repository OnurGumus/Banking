﻿module Banking.Command.API

open FCQRS.Model.Data
open FCQRS.Actor
open Banking.Model.Command.Accounting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open FCQRS.Common

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
    let actorFactories = Command.Domain.ActorFactories.factories env actorApi
    let accountSubs =  actorApi.CreateCommandSubscription actorFactories.AccountFactory
    let transferSubs =  actorApi.CreateCommandSubscription actorFactories.TransferFactory

    { new IAPI with
        member _.ActorApi = actorApi        
        member this.Deposit cid: Deposit = 
            AccountingHandler.deposit (accountSubs  cid)
        member this.Transfer cid: Transfer = 
            TransferHandler.transfer (transferSubs  cid)
        member this.Withdraw cid: Withdraw = 
            AccountingHandler.withdraw (accountSubs  cid)
    }