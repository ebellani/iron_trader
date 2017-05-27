module BlinktradeApi

open System
open System.Net.Http
open System.Security.Cryptography
open System.Text
open FSharpx.Choice
open FSharpx.Validation
open FSharpx.Collections
open Logary
open Logary.Message
open BlinktradeApiTypes
open LedgerTypes

module DataReader =

    let dateTimeFormat = "yyyy-MM-dd HH:m:s"
    /// These are keys used in a RawEntry
    let operationKey   = "Operation"
    let amountKey      = "Amount"
    let createdKey     = "Created"
    let descriptionKey = "Description"
    let referenceKey   = "Reference"
    let ledgerIdKey    = "LedgerID"
    let currencyKey    = "Currency"
    /// the value below is the divisior that needs to be applied to
    /// amounts that come from the API, for some reason.
    let apiAmountDivisor  = decimal 1e+8

    let readDate format dateTimeString =
        try
            Success(DateTime.ParseExact(dateTimeString,
                                        format,
                                        Globalization.CultureInfo.InvariantCulture))
        with
            | :? FormatException as e ->
                Failure(UnexpectedDateFormat
                           (sprintf
                                "Format expected:'%s', entry was '%s'. Error message was '%s'"
                                format
                                dateTimeString
                                e.Message))

    let readOperation = function
        | "C" -> Success Credit
        | "D" -> Success Debit
        | opString -> Failure(UnrecognizedOperation(
                               sprintf "Unrecognized operation string: '%A'"
                                       opString))

    let makeLabel entryType operation =
        let maybeAddRefund defaultOperation entryText =
            match defaultOperation with
                | defOp when defOp = operation -> entryText
                | _ -> sprintf "%s refund" entryText
        match entryType with
            | Deposit       -> maybeAddRefund Credit "Deposit"
            | DepositFee    -> maybeAddRefund Debit  "Deposit Fee"
            | Bonus         -> maybeAddRefund Credit "Bonus"
            | Withdrawal    -> maybeAddRefund Debit  "Withdrawal"
            | WithdrawalFee -> maybeAddRefund Debit  "Withdrawal Fee"
            | TradeFee      -> maybeAddRefund Debit  "Trade Fee"
            | Trade         -> "Traded currencies"

    let readEntryType = function
        | "D"  -> Success Deposit
        | "DF" -> Success DepositFee
        | "B"  -> Success Bonus
        | "W"  -> Success Withdrawal
        | "WF" -> Success WithdrawalFee
        | "TF" -> Success TradeFee
        | "T"  -> Success Trade
        | entryType ->
            Failure(UnrecognizedEntryType (sprintf "Unrecognized entry type string: '%s'" entryType))

    let readAmount amountString =
        try
            Success((decimal amountString) / apiAmountDivisor)
        with
            | :? FormatException as e ->
            Failure(UnrecognizedAmount (sprintf "Unrecognized amount string: '%s'. The error was: '%s'" amountString e.Message))


    /// Builds a raw entry by mapping the columns to the values returned.
    let buildRawEntryMap columns entryList : RawEntry =
        Array.zip columns entryList |> Map.ofArray

    let isLastPage (entriesArray:Array) (pageSize:int) =
        entriesArray.Length < pageSize

    let readLedgerResponse requestResponse =
        try
            Success(LedgerResponse.Parse requestResponse)
        with
            | e -> Failure(JsonParseError (sprintf "Error reading ledger response: '%s'" e.Message))

    let readRawEntry columns entry  =
        buildRawEntryMap columns entry

    /// This is the function that ties all the entry reading into a
    /// single point and produces an actual Entry.
    let readRegularEntry (rawEntry:RawEntry) =
        let tryFindValue column =
            let error = MissingColumn
                            (sprintf "Could not find column '%A' in entry '%A'."
                                 column
                                 rawEntry)
            let maybeResult = rawEntry.TryFind column
            ofOption error maybeResult
        choose {
            let! rawOperation   = tryFindValue operationKey
            let! rawAmount      = tryFindValue amountKey
            let! rawCreated     = tryFindValue createdKey
            let! rawDescription = tryFindValue descriptionKey
            let! reference      = tryFindValue referenceKey
            let! id             = tryFindValue ledgerIdKey
            let! currency       = tryFindValue currencyKey
            let! operation      = readOperation rawOperation
            let! date           = readDate dateTimeFormat rawCreated
            let! entryType      = readEntryType rawDescription
            let! amount         = readAmount rawAmount
            let label           = makeLabel entryType operation
            return! Success({ Date      = date;
                              Operation = operation;
                              Label     = label;
                              Currency  = currency;
                              LedgerId  = id;
                              Amount    = amount;
                              EntryType = entryType
                              Reference = reference})}

module Request =

    let logger = Logging.getCurrentLogger ()
    let ledgerMessageCode : MessageCode = "U34"
    let baseApiUrl                      = "https://api.blinktrade.com"
    let apiVersion                      = "v1"
    let defaultPageSize                 = 20
    let foxbit : BrokerId               = 4
    let timeout                         = 10<Sec>

    /// A nonce is needed by the API, see
    /// https://blinktrade.com/docs/#trade-api
    ///
    /// Repeated from the above:
    ///
    /// Nonce: Must be an integer, always greater than the
    /// previous one
    let getNounce () = string(DateTime.Now.Ticks)

    /// Both the functions below are from
    /// https://stackoverflow.com/a/24403500/7412540, and their
    /// purpose is to provide the signature for the api.
    let hexDigest (bytes : byte[]) : Nonce =
       let sb = System.Text.StringBuilder()
       bytes |> Array.iter (fun b -> b.ToString("X2") |> sb.Append |> ignore)
       string sb

    let getSignature (secret : Secret) (message : Nonce) : Signature =
       use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secret))
       hmac.ComputeHash(Encoding.UTF8.GetBytes(message))
       |> hexDigest

    let makeHeaders apiKey secret =
        let nonce = getNounce()
        { UserAgent   = "iron_trade/0.1";
          ContentType = "application/json";
          ApiKey      = apiKey;
          Nonce       = nonce;
          Signature   = getSignature secret nonce }

    let makeRequestId () : RequestId =
        let rnd = Random()
        rnd.Next()

    /// Creates the standard ledger message request. See
    /// https://blinktrade.com/docs/#ledger
    ///     // JsonConvert.SerializeObject(msg)
    let makeLedgerRequestMessage pageNumber =
        LedgerRequest.Root(ledgerMessageCode,
                           makeRequestId(),
                           pageNumber,
                           defaultPageSize,
                           foxbit)

    /// Extract the actual responses out of the body of the request to the
    /// API.
    let readRequestResponse jsonStringResponse =
        try
            let requestResponse = RequestResponse.Parse jsonStringResponse
            let status = requestResponse.Status
            match status with
                | 200 ->
                    let responses = requestResponse.Responses
                    match responses with
                        | [||] -> Failure(EmptyResponse)
                        | _    -> Success(requestResponse.Responses.[0].ToString())
                | _ -> Failure(FailureCode (sprintf "Error status from API: '%A'. Description of error: '%A'." status requestResponse.Description))
        with
        | e ->
            Failure(JsonParseError (sprintf "Error reading request response: '%s'" e.Message))

    let sendMessage apiKey secret message =
        let client = new HttpClient()
        client.Timeout <- TimeSpan(0,0, int(timeout))
        client.BaseAddress <- Uri(baseApiUrl)
        let url = sprintf "tapi/%s/message" apiVersion
        let request = new HttpRequestMessage(HttpMethod.Post, url)
        logger.info (eventX (sprintf "Requesting post to URL '%s' message '%s'" url message))
        match makeHeaders apiKey secret with
            | { UserAgent = ua;
                ContentType = ct;
                ApiKey = key;
                Nonce = nonce;
                Signature = signature } ->
                let headers = client.DefaultRequestHeaders
                headers.Accept.Add(
                    Headers.MediaTypeWithQualityHeaderValue(ct))
                headers.Add("APIKey", key)
                headers.Add("Nonce", nonce)
                headers.Add("Signature", signature)
                headers.Add("User-Agent", ua)
                request.Content <- new StringContent(
                    message,
                    Encoding.UTF8,
                    "application/json");
                async {
                    try
                        let! response = Async.AwaitTask(client.SendAsync(request))
                        let! body = Async.AwaitTask(response.Content.ReadAsStringAsync())
                        logger.info (eventX "Post was successful")
                        return Success body
                    with
                    | :? HttpRequestException as e ->
                    return Failure(HttpError(sprintf "HTTP error while sending message '%s' -> '%s'" message e.Message))
                    | e -> return Failure(UnknownRequestError(sprintf "Unkown error while sending message '%s' -> '%s'" message e.Message))
                } |> Async.RunSynchronously

    let getLedgerResponse apiKey secret pageNum =
        let message = makeLedgerRequestMessage pageNum
        let jsonMessage = message.JsonValue.ToString()
        choose {
            let! responseBody = sendMessage apiKey secret jsonMessage
            let! requestResponse = readRequestResponse responseBody
            return! DataReader.readLedgerResponse requestResponse
            }

    /// This is the main mechanism to fetch entries from the ledger
    /// reponses.
    let ledgerResponseToEntries = function
        | Success (ledgerResponse:LedgerResponse.Root) ->
            // logger.info (eventX (sprintf "Generating entries from ledgerResponse %A" ledgerResponse))
            let columns = ledgerResponse.Columns
            Array.map
                (fun (jsonEntryData:LedgerResponse.NumbersOrStringsOrDateTime) ->
                  jsonEntryData.Strings
                  |> DataReader.readRawEntry columns
                  |> DataReader.readRegularEntry)
                 ledgerResponse.LedgerListGrp
            |> Seq.ofArray
        | Failure lr -> Seq.singleton (Failure lr)

    /// This represents a sequence of requests from the API. Each
    /// element of this sequence is an attempt to retrieve data from
    /// the API. As such, it only progresses in page numbers if a
    /// success is achieved. An empty successful page is considered
    /// the last page.
    let getLedgerResponseGenerator apiKey secret =
        let ledgerResponseGetter = getLedgerResponse apiKey secret
        let generator pageNumber =
            logger.debug (eventX (sprintf "Getting page %A" pageNumber))
            let ledgerResponse = ledgerResponseGetter pageNumber
            match ledgerResponse with
            // progress if its a success
            | Success (lr:LedgerResponse.Root) ->
                if lr.LedgerListGrp.Length = 0 then
                    None
                else
                    Some(ledgerResponse, pageNumber + 1)
            // retry the same page if its a failure.
            | Failure _ -> Some (ledgerResponse, pageNumber)
        generator
