module Banking.Application.Command.Accounting
open Banking.Model.Command.Accounting
open FCQRS.Model.Data

[<Interface>]
type IAccounting =
    abstract Deposit: CID -> Deposit
    abstract Withdraw: CID -> Withdraw
    abstract Transfer: CID -> Transfer
        
        
