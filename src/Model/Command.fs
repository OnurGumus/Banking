module Banking.Model.Command
open Banking.Model.Data
open FCQRS.Model.Data

module Accounting =
    type Deposit =  UserIdentity  ->  AccountName -> Async<Result<Version, string list>>
    type Withdraw =  UserIdentity  ->  AccountName -> Async<Result<Version, string list>>
    type Transfer =  UserIdentity  ->  AccountName -> AccountName -> Async<Result<Version, string list>>