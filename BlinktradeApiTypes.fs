module BlinktradeApiTypes
open FSharp.Data
open System
open LedgerTypes
[<Measure>] type Sec
type Key             = String

type Secret          = String

type Signature       = String

type Nonce           = String

type Headers         = { UserAgent    : String
                         ContentType  : String
                         ApiKey       : String
                         Nonce        : Nonce
                         Signature    : Signature }
type RequestId       = int

type MessageCode     = String

type BrokerId        = int

type RequestResponse = JsonProvider<"./data/request-response.json">

type LedgerResponse  = JsonProvider<"./data/ledger-response.json">

type LedgerRequest   = JsonProvider<"./data/ledger-request.json">

type RawEntry        = Map<string,string>

/// These are all the errors that can arise from interacting with the
/// API.
type ApiError =
    | FailureCode of String
    | HttpError of String
    | UnexpectedDateFormat  of string
    | UnrecognizedEntryType of string
    | UnrecognizedOperation of string
    | UnrecognizedAmount of string
    | MissingColumn of string
    | JsonParseError of string
    | EmptyResponse
    | UnknownEntryError
    | UnknownRequestError of string

type SystemError =
    | ApiError of ApiError
    | DomainError of DomainError
