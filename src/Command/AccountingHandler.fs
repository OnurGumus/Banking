module internal AccountingHandler

open Banking.Command
open Banking.Model.Command.Accounting
open Banking.Model.Data
open FCQRS.Common
open FCQRS.Model.Aether
open FCQRS.Model.Aether.Operators
open FCQRS.Model.Data
open Domain.Account


let deposit createSubs : Deposit =
    fun  operationDetails ->


        let actorId  =  operationDetails.AccountName ^. (Lens.toValidated AccountName.Value_ >-> ShortString.Value_  )
        async {
            let! subscribe =
                createSubs actorId (Deposit(operationDetails)) 
                    (fun (e: Event) ->e.IsBalanceUpdated || e.IsAccountMismatch  )
            match subscribe with
            | {
                  EventDetails = BalanceUpdated _
                  Version = v
              } -> 
                return  v |> ValueLens.TryCreate |> Result.mapError (fun e -> [e.ToString()])
            | {
                  EventDetails =   _
                  Version = v
              } -> return   Error [sprintf "Deposit failed for account %s" <| actorId.ToString()]
        }


let withdraw createSubs : Withdraw =
    fun operationDetails ->

        
        let actorId  =  operationDetails.AccountName ^. (Lens.toValidated AccountName.Value_ >-> ShortString.Value_  )
        async {
            let! subscribe =
                createSubs actorId (Withdraw(operationDetails)) 
                    (fun (e: Event) ->e.IsAccountMismatch || e.IsBalanceUpdated || e.IsOverdraftAttempted)
            match subscribe with
            | {
                    EventDetails = BalanceUpdated _
                    Version = v
                } -> 
                return  v |> ValueLens.TryCreate |> Result.mapError (fun e -> [e.ToString()])
            | {
                    EventDetails =   _
                    Version = v
                } -> return   Error [sprintf "Deposit failed for account %s" <| actorId.ToString()]
        }

