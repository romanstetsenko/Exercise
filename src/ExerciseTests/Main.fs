module ExerciseTests
open Expecto

[<EntryPoint>]
let main argv =
    let cfg = { 
        defaultConfig with 
            verbosity = Expecto.Logging.LogLevel.Verbose
    }
    //let argv = [|"--filter-test-case"; "it fails when every next sentence starts with a lowercase letter"|]
    Tests.runTestsInAssembly cfg argv
