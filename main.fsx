
#load "references-fixed.fsx"  
#I "/workspaces/Banking/src/Query/libs"
open System
open System.IO
open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging
open Hocon.Extensions.Configuration
open Banking.Application.Command.Accounting
open FCQRS.Model.Data
open FCQRS.Model.Aether.Operators
open FCQRS.Model.Aether
open Banking.Model.Data
open Banking.Model.Command.Accounting


let tempFile = "/workspaces/Banking/src/Server/Database/Banking.db" //Path.GetTempFileName()
let connString = $"Data Source={tempFile}"


let configBuilder =
    ConfigurationBuilder()
        .AddEnvironmentVariables()
        .AddHoconFile("/workspaces/Banking/src/Server/config.hocon")
        .AddInMemoryCollection(
            dict
                [| "config:connection-string", connString
                   "config:akka:persistence:journal:sql:connection-string", connString
                   "config:akka:persistence:snapshot-store:sql:connection-string", connString
                   "config:akka:persistence:query:journal:sql:connection-string", connString |]
        )
        

let config = configBuilder.Build()

let lf = LoggerFactory.Create(fun builder -> builder.AddConsole().AddDebug() |> ignore)

let env = new Banking.Server.Environments.AppEnv(config,lf)

env.Init()
open FCQRS.Model.Aether.Operators

let acc = env :> IAccounting


let cid:CID = "123" |> ValueLens.CreateAsResult |> Result.value

let money :Money =  ValueLens.Create  10
let deposit : Deposit = acc.Deposit cid 
let userIdentity: UserIdentity = ValueLens.Create <| Guid.NewGuid()
let accountName: AccountName =  "123"  |> ValueLens.CreateAsResult |> Result.value
  

let depositResult = deposit  userIdentity accountName money |> Async.RunSynchronously

printfn "%A" depositResult

let money2 :Money =  ValueLens.Create  7

let withdraw : Withdraw = acc.Withdraw cid 
let withdrawResult = withdraw  userIdentity accountName money2 |> Async.RunSynchronously
printfn "%A" withdrawResult

let withdrawResultFailed = withdraw  userIdentity accountName money2 |> Async.RunSynchronously
printfn "%A" withdrawResultFailed