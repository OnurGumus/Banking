module Banking.Application.Event
open Banking.Model.Data

type AccountEvent =
    | BalanedUpdated of Account
type TransferDetails = { From: AccountName; To: AccountName; Amount: PositiveMoney }

type TransferEvent =
    | TransferCompleted of TransferDetails

type DataEvent = 
    | AccountEvent of AccountEvent
    | TransferEvent of TransferEvent
