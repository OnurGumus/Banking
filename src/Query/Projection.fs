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
open Microsoft.Extensions.Logging

type CID = FCQRS.Model.Data.CID

let handleEventWrapper env (ctx: Sql.dataContext) (actorApi: IActor) (subQueue: ISourceQueue<_>) (envelop: EventEnvelope) =
    let loggingFactory = env :> ILoggerFactory
    let logger = loggingFactory.CreateLogger("Banking.Query.Projection")
    try
        //Log.Debug("Envelop:{@envelop}", envelop)
        let offsetValue = (envelop.Offset :?> Sequence).Value
        let dataEvent =
            match envelop.Event with

            | :? Event<Account.Event> as  event ->
                AccountProjection.handle ctx event
            | :? Event<Transfer.Event> as  event ->
                TransferProjection.handle ctx event

            | _ -> None

        let offset = ctx.Main.Offsets.Individuals.Banking
        offset.OffsetCount <- offsetValue
        ctx.SubmitUpdates()

        match (dataEvent: DataEvent<DataEvent> option) with
        | Some dataEvent -> subQueue.OfferAsync(dataEvent).Wait()
        | _ -> ()
    with
    | ex ->
       logger.LogCritical(ex, "Error handling event: {@envelop}", envelop)
       actorApi.System.Terminate().Wait()
       System.Environment.Exit(-1)


let init env (connectionString: string) (actorApi: IActor) query =
        let ctx = Sql.GetDataContext(connectionString)
    
        use conn = ctx.CreateConnection()
        conn.Open()
        let cmd = conn.CreateCommand()
        cmd.CommandText <- "PRAGMA journal_mode=WAL;"
        cmd.ExecuteNonQuery() |> ignore
    
        let offsetCount =  ctx.Main.Offsets.Individuals.Banking.OffsetCount
        FCQRS.Query.init actorApi offsetCount (handleEventWrapper env ctx) query
       