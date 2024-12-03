module Banking.Model.Command
open Banking.Model.Data
open FCQRS.Model.Data

module Accounting =
    type Deposit =  UserIdentity  ->  AccountName ->  Money ->Async<Result<Version, string list>>
    type Withdraw =  UserIdentity  ->  AccountName -> Money ->Async<Result<Version, string list>>
    type Transfer =  UserIdentity  ->  AccountName ->  Money-> AccountName -> Async<Result<Version, string list>>