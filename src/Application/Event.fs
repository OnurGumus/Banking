module Banking.Application.Event
open Banking.Model.Data

type AccountEvent =
    | BalanedUpdated of Account

type DataEvent = 
    | AccountEvent of AccountEvent