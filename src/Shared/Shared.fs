namespace Shared        
        
type Counter = int

type Request =
    | ConvertToCsv of string
    | ConvertToXml of string

module Routes =
    module Api =
        let init = "/api/init"
        let transform = "/api/transform"
