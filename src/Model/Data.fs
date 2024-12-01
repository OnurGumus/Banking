module rec Banking.Model.Data

open FCQRS.Model.Validation
open FsToolkit.ErrorHandling
open FCQRS.Model.Data
open System
open FCQRS.Model.Aether
open FCQRS.Model.Aether.Operators

type UserIdentity = 
    private  UserIdentity of Guid
        static let zero =  Guid.Empty |> UserIdentity
        static member Value_ : Lens<UserIdentity,Guid> = 
            (fun (UserIdentity u) -> u), 
            (fun (g: Guid) _ ->   g |> UserIdentity)

        static member CreateNew() =
            zero |> (Guid.NewGuid())  ^=  (UserIdentity.Value_ ) 
        
        override this.ToString() = 
            (ValueLens.Value this).ToString()

type AccountName =
    private AccountName of ShortString
        static member Value_ : Lens<AccountName,ShortString> = 
            (fun (AccountName u) -> u), 
            (fun (g: ShortString) _ ->   g |> AccountName)

        override this.ToString() = 
            (ValueLens.Value this).ToString()


type Money = 
    private Money of decimal
        static let zero =  0.0M |> Money
        static member Value_ : Lens<Money,decimal> = 
            (fun (Money u) -> u), 
            (fun (g: decimal) _ ->   g |> Money)

        static member CreateNew() =
            zero |> (0.0M)  ^=  (Money.Value_ ) 
        
        override this.ToString() = 
            (ValueLens.Value this).ToString()