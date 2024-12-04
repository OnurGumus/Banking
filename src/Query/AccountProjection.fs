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
    | Account.DepositSuccess details ->
        let accountName = details.AccountName |> ValueLens.Value |> ValueLens.Value
        let money = details.Money |> ValueLens.Value
        let userIdentity = details.UserIdentity |> ValueLens.Value |> ValueLens.Value
        let existingRow =
            query {
                for c in (ctx.Main.Accounts) do
                    where (c.UserIdentity = userIdentity && c.AccountName = accountName)

                    take 1
                    select c
            }
            |> Seq.tryHead
        match existingRow with
        | Some row ->
            row.Balance <- row.Balance + money
        | None ->
          let row = 
            ctx.Main.Accounts.``Create(CreatedAt, Document, UpdatedAt, Version)`` (
                System.DateTime.UtcNow,
                encodeToBytes details,
                System.DateTime.UtcNow,
                0L)
          row.AccountName <-  accountName
          row.Balance <- money
          row.UserIdentity <- userIdentity

        Some {
            Type = AccountEvent(Deposited details)
            CID = cid
        } 
    | Account.WithdrawSuccess details ->
        let accountName = details.AccountName |> ValueLens.Value |> ValueLens.Value
        let money = details.Money |> ValueLens.Value
        let userIdentity = details.UserIdentity |> ValueLens.Value |> ValueLens.Value
        let existingRow =
            query {
                for c in (ctx.Main.Accounts) do
                    where (c.UserIdentity = userIdentity && c.AccountName = accountName)

                    take 1
                    select c
            }
            |> Seq.tryHead
        match existingRow with
        | Some row ->
            row.Balance <- row.Balance - money
        | None ->
          let row = 
            ctx.Main.Accounts.``Create(CreatedAt, Document, UpdatedAt, Version)`` (
                System.DateTime.UtcNow,
                encodeToBytes details,
                System.DateTime.UtcNow,
                0L)
          row.AccountName <-  accountName
          row.Balance <- money
          row.UserIdentity <- userIdentity

        Some {
            Type = AccountEvent(Withdrawn details)
            CID = cid
        } 
    | _ -> None

            