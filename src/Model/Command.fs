module Banking.Model.Command
open Banking.Model.Data
open FCQRS.Model.Data

module Accounting =
    type Deposit =  OperationDetails ->Async<Result<Version, string list>>
    type Withdraw =  OperationDetails ->Async<Result<Version, string list>>
    type Transfer =   TransferDetails -> Async<Result<Version, string list>>