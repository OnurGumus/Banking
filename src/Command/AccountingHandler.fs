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
                    (fun (e: Event) ->e.IsDepositSuccess || e.IsDepositFailed)
            match subscribe with
            | {
                  EventDetails = DepositSuccess _
                  Version = v
              } -> 
                return  v |> ValueLens.TryCreate |> Result.mapError (fun e -> [e.ToString()])
            | {
                  EventDetails =  DepositFailed _
                  Version = v
              } -> return   Error [sprintf "Deposit failed for account %s" <| actorId.ToString()]
            | e -> return Error [sprintf "Unexpected event %A" e]
        }


let withdraw createSubs : Deposit =
    fun operationDetails ->

        
        let actorId  =  operationDetails.AccountName ^. (Lens.toValidated AccountName.Value_ >-> ShortString.Value_  )
        async {
            let! subscribe =
                createSubs actorId (Withdraw(operationDetails)) 
                    (fun (e: Event) ->e.IsDepositSuccess || e.IsDepositFailed)
            match subscribe with
            | {
                    EventDetails = WithdrawSuccess _
                    Version = v
                } -> 
                return  v |> ValueLens.TryCreate |> Result.mapError (fun e -> [e.ToString()])
            | {
                    EventDetails =  WithdrawFailed _
                    Version = v
                } -> return   Error [sprintf "Deposit failed for account %s" <| actorId.ToString()]
            | e -> return Error [sprintf "Unexpected event %A" e]
        }

