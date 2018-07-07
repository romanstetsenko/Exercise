namespace Shared        
        
type Counter = int

type Request =
    | ConvertToCsv of string
    | ConvertToXml of string
