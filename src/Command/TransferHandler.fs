module internal TransferHandler

open Banking.Command
open Banking.Model.Command.Accounting
open Banking.Model.Data
open FCQRS.Common
open FCQRS.Model.Aether
open FCQRS.Model.Aether.Operators
open FCQRS.Model.Data
open Domain.Transfer
open System


let transfer createSubs : Transfer =
    fun  transferDetails ->

        let actorId  =  Guid.NewGuid().ToString()
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



