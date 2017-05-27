module Ledger

open BlinktradeApiTypes
open LedgerTypes
open System
open System.IO
open LedgerTypes
open FSharpx.Validation
open Logary
open Logary.Message

module Fx =
    let dateFormat = "yyyy/MM/dd"
    let indentation = "    "

    let buildFxEntry (account:Account)
                     { Currency  = c1;
                       Operation = op1;
                       Label     = l1;
                       Date      = d1;
                       Amount    = am1;
                       Reference = ref1;
                       LedgerId  = id1 }
                     { Currency  = c2;
                       Operation = op2;
                       Amount    = am2;
                       Reference = ref2 } =
        match (op1, op2) with
        | Credit, Debit when ref1 = ref2 ->
            Success {
                Account        = account;
                Date           = d1;
                Label          = l1 ;
                LedgerId       = id1;
                CreditCurrency = c1;
                DebitCurrency  = c2;
                CreditAmount   = am1;
                DebitAmount    = am2;
                Reference1     = ref1;
                Reference2     = ref2;
            }
        | Debit, Credit when ref1 = ref2 ->
            Success {
                Account        = account;
                Date           = d1;
                Label          = l1;
                LedgerId       = id1;
                CreditCurrency = c2;
                DebitCurrency  = c1;
                CreditAmount   = am2;
                DebitAmount    = am1;
                Reference1     = ref1;
                Reference2     = ref2;
            }
        | _, _ when (ref1 <> ref2) ->
            Failure (FxRecordsWithDifferentReferences
                         (sprintf "References '%s' and '%s'" ref1 ref2))
        | _  -> Failure FxRecordsWithEqualOperations

    let toString = function
        | { Account = account;
            Date = date;
            Label = label;
            DebitCurrency = dc
            CreditCurrency = cc;
            CreditAmount = ca;
            DebitAmount = da } ->
            sprintf "%s * %s\n%s%s  %s$ %.8f\n%s%s  %s$ %.8f\n"
                (date.ToString dateFormat)
                label
                indentation account cc ca
                indentation account dc (- da)

module Regular =
    let dateFormat = Fx.dateFormat
    let header = "Date, Label, Amount"

    let isBtc { Currency = curr } =
        match curr with
        | "BTC" -> true
        | _ -> false


    let toString = function
        | { Operation = operation;
            Date = date;
            Label = label;
            Amount = amount }
            ->
            sprintf "%s, \"%s\", %.8f"
                (date.ToString dateFormat)
                label
                (match operation with
                 | Credit -> amount
                 | Debit  -> (- amount))


module Core =


    let logger = Logging.getCurrentLogger ()

    let isPartOfFxEntry = function
    |  { EntryType = entryType } when entryType = Trade -> true
    | _ -> false

    let tryHead seq =
        try
            Some (List.head seq)
        with
            | :? ArgumentException as e ->
                None

    /// This is where the processing of the entry sequence take
    /// place.
    let rec processEntries (fxEntriesStream:StreamWriter)
                           (fiatEntriesStream:StreamWriter)
                           (btcEntriesStream:StreamWriter)
                           account
                           (entries:List<Choice<RegularEntry,ApiError>>)
                           maybePreviousFxEntry
                           : Choice<unit, SystemError>  =
        match (tryHead entries), maybePreviousFxEntry with
        | None, None ->
             // finished processing
            Success ()
        | None, Some _  ->
            // no entries left, but there is a dangling fxEntry
            Failure (DomainError MissingFxEntryPart)
        | Some(Success fxEntrySecondPart), Some fxEntryFirstPart when isPartOfFxEntry fxEntrySecondPart ->
            // a full Fx entry is expected here, since both
            // entries are parts of one and are sequential. They
            // can still have different references, though.
            match Fx.buildFxEntry account fxEntryFirstPart fxEntrySecondPart with
            | Success fxEntry ->
                // this is a FX entry, write it as Ledger
                fxEntry     |>
                Fx.toString |>
                fxEntriesStream.WriteLine |>
                ignore
                processEntries fxEntriesStream
                               fiatEntriesStream
                               btcEntriesStream
                               account
                               (List.tail entries)
                               None
            | Failure e ->
                    Failure (DomainError e)
        | Some(Success _), Some _  ->
            // there is an fx entry, but the next entry is not the
            // next part of it. This is an error.
            Failure (DomainError MissingFxEntryPart)
        | Some(Success fxEntryFirstPart), None when isPartOfFxEntry fxEntryFirstPart ->
            // found the first part of an FxEntry. Loop with it.
            processEntries fxEntriesStream
                           fiatEntriesStream
                           btcEntriesStream
                           account
                           (List.tail entries)
                           (Some fxEntryFirstPart)
        | Some(Success regularEntry), None ->
            // this is a regular entry, write it as CSV
            let streamToWrite = if Regular.isBtc regularEntry then
                                   btcEntriesStream else
                                   fiatEntriesStream
            regularEntry |>
            Regular.toString  |>
            streamToWrite.WriteLine |>
            ignore
            processEntries fxEntriesStream
                           fiatEntriesStream
                           btcEntriesStream
                           account
                           (List.tail entries)
                           None
        | Some(Failure e), _ ->
            Failure (ApiError e)
        | _, _ -> Failure (DomainError UnkownError)



    let startProcessing apiKey secret account path =
        let initialPage = 0
        let ledgerResponseGenerator = BlinktradeApi.Request.getLedgerResponseGenerator
                                          apiKey
                                          secret
        let ledgerResponses = Seq.unfold ledgerResponseGenerator initialPage
        let entries = Seq.collect BlinktradeApi.Request.ledgerResponseToEntries ledgerResponses |> Seq.toList
        let fxStream = new StreamWriter(path+"/fx.ledger", false)
        let regularStream = new StreamWriter(path+"/regular.csv", false)
        let btcStream = new StreamWriter(path+"/btc.csv", false)
        fxStream.AutoFlush      <- true
        btcStream.AutoFlush     <- true
        regularStream.AutoFlush <- true
        processEntries fxStream regularStream btcStream account entries None
