module Banking.Query.API

open Microsoft.Extensions.Configuration
open Banking.Model.Data
open FCQRS.Serialization
open SqlProvider
open FSharp.Data.Sql

let queryApi env (config: IConfiguration) actorApi =

    let connString = config.GetSection("config:connection-string").Value

    let query
        (
            ty: System.Type,
            filter,
            orderby,
            orderbydesc,
            thenby,
            thenbydesc,
            take: int option,
            skip,
            (cacheKey: string option)
        ) : Async<obj seq> =

        let ctx = Sql.GetDataContext(connString)

        let augment db =
            FCQRS.SQLProvider.Query.augmentQuery filter orderby orderbydesc thenby thenbydesc take skip db

        let res =
            task{
                if ty = typeof<Account> then
                    let q =
                        query {
                            for c in ctx.Main.Accounts do
                                select c
                        }
                    let! m =   augment <@ q @> |> Seq.executeQueryAsync
                    return m  |> Seq.map (fun x -> x.Document |> decodeFromBytes<Account> :> obj)
                    
                else
                    return failwith "not implemented"
            }
                
        res |> Async.AwaitTask

    Projection.init env connString actorApi query