module internal TransferHandler

open Banking.Command
open Banking.Model.Command.Accounting
open FCQRS.Common
open FCQRS.Model.Data
open Domain.Transfer
open System

let transfer createSubs : Transfer =
    fun  transferDetails ->
        let actorId  =  "Transfer_" + "1"
        async {
            let! subscribe =
                createSubs actorId (Transfer(transferDetails)) 
                    (fun (e: Event) ->e.IsAnotherTransferIsInProgress || e.IsMoneyTransferred || e.IsTransferAborted  )
            match subscribe with
            | {
                  EventDetails = MoneyTransferred _
                  Version = v
              } -> 
                return  v |> ValueLens.TryCreate |> Result.mapError (fun e -> [e.ToString()])
            | {
                  EventDetails =   _
                  Version = v
              } -> return   Error [sprintf "TransferFailed failed for account %s" <| actorId.ToString()]
        }



