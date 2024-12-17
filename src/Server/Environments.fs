module Banking.Server.Environments
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration
open Banking.Application.Command.Accounting
open FCQRS.Model.Query
open Banking.Application.Event

type AppEnv(config: IConfiguration, loggerFactory: ILoggerFactory)  as self=

    let mutable commandApi = Unchecked.defaultof<_>
    let mutable queryApi = Unchecked.defaultof<_>

    interface ILoggerFactory with
        member this.AddProvider(provider: ILoggerProvider) : unit = loggerFactory.AddProvider(provider)

        member this.CreateLogger(categoryName: string) : ILogger =
            loggerFactory.CreateLogger(categoryName)

        member this.Dispose() : unit = loggerFactory.Dispose()

    interface IQuery<DataEvent> with
            member _.Query<'t>(?filter, ?orderby, ?orderbydesc, ?thenby, ?thenbydesc, ?take, ?skip, ?cacheKey) =
                async {
                    let! res =
                        queryApi.Query(
                            ty = typeof<'t>,
                            ?filter = filter,
                            ?orderby = orderby,
                            ?orderbydesc = orderbydesc,
                            ?thenby = thenby,
                            ?thenbydesc = thenbydesc,
                            ?take = take,
                            ?skip = skip,
                            ?cacheKey = cacheKey
    
                        )
                    return res |> Seq.cast<'t> |> List.ofSeq
                }
            member _.Subscribe(cb, cancellationToken) = 
                let ks = queryApi.Subscribe(cb)
                cancellationToken.Register(fun _ ->ks.Shutdown()) 
            member _.Subscribe(filter, take, cb, cancellationToken) = 
                let ks, res = queryApi.Subscribe(filter, take, cb)
                let d= cancellationToken.Register(fun _ ->ks.Shutdown()) 
                async {
                    do! res
                    return d
                }
        
        
    
    interface IAccounting with
        member _.Deposit cid = commandApi.Deposit cid
        member _.Withdraw  cid= commandApi.Withdraw cid
        member _.Transfer cid = commandApi.Transfer cid


    interface IConfiguration with
        member _.Item
            with get (key: string) = config.[key]
            and set key v = config.[key] <- v

        member _.GetChildren() = config.GetChildren()
        member _.GetReloadToken() = config.GetReloadToken()
        member _.GetSection key = config.GetSection(key)

    member this.Reset() = 
                Migrations.reset config
                this.Init()
        
    member _.Init() = 
        Migrations.init config
        commandApi <- Banking.Command.API.api self
        queryApi <- Banking.Query.API.queryApi  self config commandApi.ActorApi
        

    