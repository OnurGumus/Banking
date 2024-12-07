module TransferProjection

open FSharp.Data.Sql.Common

open Banking.Model.Data
open FCQRS.Serialization
open FCQRS.Model
open Akka.Persistence.Query
open FCQRS.Model.Query
open Banking.Command.Domain
open SqlProvider
open FCQRS.Model.Data
open Banking.Application.Event

// QueryEvents.SqlQueryEvent
// |> Event.add (fun query -> printfn "Executing SQL {query}: %A" query)

//printfn "SqlProvider loaded"

let handle (ctx: Sql.dataContext)(e:FCQRS.Common.Event<Transfer.Event>) =
    let eventDetails = e.EventDetails
    let cid = e.CorrelationId

    match eventDetails with
    | Transfer.Event.MoneyTransferred x ->
        let details : Banking.Application.Event.TransferDetails = 
            { From = x.From; To = x.To; Amount = x.Amount }
        Some {
            Type = TransferEvent(TransferCompleted details)
            CID = cid
        }

    | _ -> None
