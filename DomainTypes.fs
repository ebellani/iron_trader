module LedgerTypes
open System

type Operation =
    | Credit
    | Debit

type Currency = String

type Account = String

type Reference = String

type LedgerId = String

type EntryType =
    | Deposit
    | DepositFee
    | Bonus
    | Withdrawal
    | WithdrawalFee
    | TradeFee
    | Trade

/// This is an entry that occurs within a single currency
type RegularEntry = { Currency  : Currency
                      Operation : Operation
                      Date      : DateTime
                      Label     : String
                      Amount    : Decimal
                      EntryType : EntryType
                      LedgerId  : LedgerId
                      Reference : Reference}

/// Exchanging currencies entries. There is not a single currency
/// involved, thus it is not amenable to be treated the same as the
/// regular Entry. This has 2 references, since it is composed of 2
/// ledger entries in the exchange.
type FxEntry = {
    Account        : Account
    Date           : DateTime
    Label          : String
    LedgerId       : LedgerId
    DebitCurrency  : Currency
    CreditCurrency : Currency
    CreditAmount   : Decimal
    DebitAmount    : Decimal
    Reference1     : Reference
    Reference2     : Reference
    }

type LedgerEntry =
    | Regular of RegularEntry
    | Fx of FxEntry

let Success = Choice1Of2
let Failure = Choice2Of2

/// These are all the errors that can arise from interacting with the
/// API.
type DomainError =
    | InvalidFxRecords of String
    | MissingFxEntryPart
    | FxRecordsWithDifferentReferences of String
    | FxRecordsWithEqualOperations
    | UnkownError // should just a filler for helping the compiler
