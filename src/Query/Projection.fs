module internal Banking.Query.Projection

open FSharp.Data.Sql.Common
open FCQRS.Actor
open Akka.Streams
open Akka.Persistence.Query
open FCQRS.Common
open FCQRS.Model.Query
open Banking.Application.Event
open Banking.Command.Domain
open SqlProvider

type CID = FCQRS.Model.Data.CID

let handleEventWrapper (ctx: Sql.dataContext) (actorApi: IActor) (subQueue: ISourceQueue<_>) (envelop: EventEnvelope) =
    try
        //Log.Debug("Envelop:{@envelop}", envelop)
        let offsetValue = (envelop.Offset :?> Sequence).Value
        let dataEvent =
            match envelop.Event with

            | :? Event<Account.Event> as { EventDetails = eventDetails; CorrelationId = cid } ->
                AccountProjection.handle ctx eventDetails cid

            | _ -> None

        let offset = ctx.Main.Offsets.Individuals.Banking
        offset.OffsetCount <- offsetValue
        ctx.SubmitUpdates()

        match (dataEvent: DataEvent<DataEvent> option) with
        | Some dataEvent -> subQueue.OfferAsync(dataEvent).Wait()
        | _ -> ()
    with
    | ex ->
       printfn "Error: %s" ex.Message
       actorApi.System.Terminate().Wait()
       System.Environment.Exit(-1)


let init (connectionString: string) (actorApi: IActor) query =
        let ctx = Sql.GetDataContext(connectionString)
    
        use conn = ctx.CreateConnection()
        conn.Open()
        let cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA journal_mode=WAL;"
        cmd.ExecuteNonQuery() |> ignore
    
        let offsetCount =  ctx.Main.Offsets.Individuals.Banking.OffsetCount
        FCQRS.Query.init actorApi offsetCount (handleEventWrapper ctx) query
       