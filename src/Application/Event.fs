module Banking.Application.Event
open Banking.Model.Data

type AccountEvent =
    | Deposited of OperationDetails
    | Withdrawn of OperationDetails

type DataEvent = 
    | AccountEvent of AccountEvent