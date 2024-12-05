module AccountProjection

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

let handle (ctx: Sql.dataContext) eventDetails cid =
    let cid:CID = cid |> ValueLens.CreateAsResult |> Result.value

    match eventDetails with
    | Account.BalanceUpdated {Account = account;  } ->
        
        let owner = account.Owner |> ValueLens.Value |> ValueLens.Value
        let accountName = account.AccountName |> ValueLens.Value |> ValueLens.Value
        let balance = account.Balance |> ValueLens.Value 
        let serialize = encodeToBytes account
        let existingRow =
            query {
                for c in (ctx.Main.Accounts) do
                    where (c.UserIdentity = owner  && c.AccountName = accountName)
                    take 1
                    select c
            }
            |> Seq.tryHead
        match existingRow with
        | Some row ->
            row.Balance <- balance
            row.Document <- serialize
        | None ->
          let row = 
            ctx.Main.Accounts.``Create(CreatedAt, Document, UpdatedAt, Version)`` (
                System.DateTime.UtcNow,
                encodeToBytes account,
                System.DateTime.UtcNow,
                0L)
          row.AccountName <-  accountName
          row.Balance <- balance
          row.UserIdentity <- owner

        Some {
            Type = AccountEvent(BalanedUpdated account)
            CID = cid
        } 
    
    | _ -> None

            