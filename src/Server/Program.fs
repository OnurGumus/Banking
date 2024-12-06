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

//let tempFile = "/workspaces/Banking/src/Server/Database/Banking.db"
let tempFile = Path.GetTempFileName()
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
open FCQRS.Model.Query

let acc = env :> IAccounting


let cid:CID = Guid.NewGuid().ToString() |> ValueLens.CreateAsResult |> Result.value

let money :Money =  ValueLens.Create  10
let deposit : Deposit = acc.Deposit cid 
let userIdentity: UserIdentity = "my user" |> ValueLens.CreateAsResult |> Result.value 
let accountName: AccountName =  "123"  |> ValueLens.CreateAsResult |> Result.value
let postiveMoney : PositiveMoney = money |> ValueLens.TryCreate |> Result.value
let operationDetails = { UserIdentity = userIdentity; AccountName = accountName ; Money = postiveMoney} 

// let depositResult = deposit  operationDetails |> Async.RunSynchronously


// let money2 :Money =  ValueLens.Create  7
// let withdraw : Withdraw = acc.Withdraw cid 
// let withdrawResult = withdraw  operationDetails|> Async.RunSynchronously

// let withdrawResultFailed = withdraw  operationDetails |> Async.RunSynchronously
// let query = env :> IQuery<_>
// System.Threading.Thread.Sleep 1000

// let list  = query.Query<Account>() |> Async.RunSynchronously

// printf "Accounts: %A" list


let transfer : Transfer = acc.Transfer cid

let toAccountName: AccountName =  "456"  |> ValueLens.CreateAsResult |> Result.value
let toUserIdentity: UserIdentity = "my user" |> ValueLens.CreateAsResult |> Result.value
let toMoney :Money =  ValueLens.Create  5
let toPostiveMoney : PositiveMoney = toMoney |> ValueLens.TryCreate |> Result.value
let toOperationDetails = { UserIdentity = toUserIdentity; AccountName = toAccountName ; Money = toPostiveMoney}
let transferDetails = { OperationDetails = operationDetails; DestinationAccountName= toAccountName}
let transferResult = transfer  transferDetails  |> Async.RunSynchronously
