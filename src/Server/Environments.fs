module Banking.Server.Environments
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration



type AppEnv(config: IConfiguration, loggerFactory: ILoggerFactory)  as self=

    let mutable commandApi = Unchecked.defaultof<_>


    interface ILoggerFactory with
        member this.AddProvider(provider: ILoggerProvider) : unit = loggerFactory.AddProvider(provider)

        member this.CreateLogger(categoryName: string) : ILogger =
            loggerFactory.CreateLogger(categoryName)

        member this.Dispose() : unit = loggerFactory.Dispose()

    

    interface IConfiguration with
        member _.Item
            with get (key: string) = config.[key]
            and set key v = config.[key] <- v

        member _.GetChildren() = config.GetChildren()
        member _.GetReloadToken() = config.GetReloadToken()
        member _.GetSection key = config.GetSection(key)

    member this.Reset() = 
                this.Init()
        
    member _.Init() = 
        commandApi <- Banking.Command.API.api self
        

    