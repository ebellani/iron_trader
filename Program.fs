open System.IO

open FSharpx.Validation
open Hopac // conf
open Logary // normal usage
open Logary.Message // normal usage
open Logary.Configuration // conf
open Logary.Targets // conf
open Argu

let path = System.Reflection.Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName
/// for now you will need to set these to your API creditials
let key = ""
let secret = ""


[<CliPrefix(CliPrefix.Dash)>]
type BanksPossile =
    | Agiplan
    | BancoBrasil
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | D -> "Remove untracked directories in addition to untracked files"
            | F -> "Git clean will refuse to delete files or directories unless given -f."
            | X -> "Remove only files ignored by Git."

[<EntryPoint>]
let main _ =
    // this is our logManager.
    use logary =
        withLogaryManager "Console" (
            withRules
                [
                   { Rule.createForTarget "console" with level = Debug  }
                ]
            // the literate console target is used to output text in
            // different colors for different types of logs. Much
            // nicer than the normal console.
            >> withTarget (LiterateConsole.create LiterateConsole.empty "console")
          ) |> run
    let successReturnCode = 0
    let failureReturnCode = 1
    let logger = Logging.getCurrentLogger()
    logger.info (eventX "Process starting.")
    match (Ledger.Core.startProcessing key
                                       secret
                                       "Assets:Broker:Foxbit:Checking"
                                       path) with
    | Success _ ->
        logger.info (eventX "Finished processing entries.")
        successReturnCode
    | Failure error ->
        logger.error
            (eventX
                 (sprintf "Encontered an error while processing entries: %A" error))
        failureReturnCode
