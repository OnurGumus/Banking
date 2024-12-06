module rec Banking.Model.Data

open FCQRS.Model.Validation
open FsToolkit.ErrorHandling
open FCQRS.Model.Data
open System
open FCQRS.Model.Aether
open FCQRS.Model.Aether.Operators
type ModelError = 
        NegativeMoney of decimal

type UserIdentity = 
    private  UserIdentity of ShortString
        static member Value_ : Lens<UserIdentity,ShortString> = 
            (fun (UserIdentity u) -> u), 
            (fun (g: ShortString) _ ->   g |> UserIdentity)

        override this.ToString() = 
            (ValueLens.Value this).ToString()

type AccountName =
    private AccountName of ShortString
        static member Value_ : Lens<AccountName,ShortString> = 
            (fun (AccountName u) -> u), 
            (fun (g: ShortString) _ ->   g |> AccountName)

        override this.ToString() = 
            (ValueLens.Value this).ToString()

#nowarn 342
[<StructuralEquality;CustomComparisonAttribute>]
type Money = 
    private Money of decimal
        static member Value_ : Lens<Money,decimal> = 
            (fun (Money u) -> u), 
            (fun (g: decimal) _ ->   g |> Money)

        static member Zero = Money 0.0M

        static member Negate (Money a) = Money (-a)

        static member Abs (Money a) = Money (abs a)

        static member (-) (Money a, Money b) = Money (a - b)
        static member (+) (Money a, Money b) = Money (a + b)
        static member (-) (Money a, b:decimal) = Money (a - b)
        static member  (+) (Money a, b:decimal) = Money (a + b)
        static member (*) (Money a, b:decimal) = Money (a * b)
        static member (/) (Money a, b:decimal) = Money (a / b)
        interface IComparable with
            member this.CompareTo (other: obj) = 
                match other with
                | :? Money as other -> 
                    (this:> IComparable<Money>).CompareTo other
                | _ -> invalidArg "other" "Must be Money"
                
        interface IComparable<Money> with
            member this.CompareTo (NonNullQuick (other: Money) )= 
                compare (ValueLens.Value this) (ValueLens.Value other)
        override this.ToString() = 
            (ValueLens.Value this).ToString()

type PositiveMoney = 
    private PositiveMoney of Money
        static member Value_ : ValidatedLens<PositiveMoney,Money, ModelError> = 
            (fun (PositiveMoney u) -> u), 
            (fun (g: Money) _ ->   
                match g with
                | Money m when m > 0.0M -> Ok(PositiveMoney g)
                | Money m -> Error (NegativeMoney m)
            )
        static member (+) (PositiveMoney a, PositiveMoney b) = PositiveMoney (a + b)
        static member (-) (PositiveMoney a, PositiveMoney b) = a-b
        override this.ToString() = 
            (ValueLens.Value this).ToString()

type OperationDetails = { UserIdentity: UserIdentity; AccountName: AccountName; Money: PositiveMoney }
type Account = { AccountName: AccountName; Balance: Money ; Owner: UserIdentity }
type TransferDetails = { OperationDetails: OperationDetails; DestinationAccountName: AccountName }
