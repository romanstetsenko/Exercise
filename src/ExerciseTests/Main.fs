module ExerciseTests
open Expecto

[<EntryPoint>]
let main argv =
    let cfg = { defaultConfig with verbosity = Expecto.Logging.LogLevel.Verbose}
    Tests.runTestsInAssembly cfg argv
