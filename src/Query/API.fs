module Banking.Query.API

open Microsoft.Extensions.Configuration
open Banking.Model.Data
open FCQRS.Serialization
open SqlProvider

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

        let res: seq<obj> =

            if ty = typeof<Account> then
                let q =
                    query {
                        for c in ctx.Main.Accounts do
                            select c
                    }
                augment <@ q @>
                |> Seq.map (fun x -> x.Document |> decodeFromBytes<Account> :> obj)

            else
                failwith "not implemented"

        async { return res }

    Projection.init env connString actorApi query