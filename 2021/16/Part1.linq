<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

let hexMap = function
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> ""
    

let bits = 
    getData "literal"
    |> Array.head
    |> (fun s -> s.ToCharArray())
    |> Array.map hexMap
    |> (fun s -> String.Join("", s))
    
type Packet = {
    Header: PacketHeader
    Body: PacketData
}
and
    PacketHeader = {
        Version: int
        TypeId: int
    }
and
    PacketData =
    | Literal of int
    | Operator of Packet list

// packet header
// [version][type id]
// [000][000]

// packet type id 4: literal
// set of 5 bits: starts with 1 means more follow, 0 means stop reading
// [1xxxx][0yyyy]
// literal value = [xxxxyyyy]

// other packet types: operator
// length type id
// 0 -> 15 bits = total length of sub packets
// 1 -> 11 bits = number of sub packets
// [0][bits][sub packets]

type ParseResult<'a> = {
    BitsRead: int
    Element: 'a
}

let parseHeader (bits: string) =
    {
        BitsRead = 6
        Element =
            {
                Version = Convert.ToInt32(bits[..2], 2)
                TypeId = Convert.ToInt32(bits[3..5], 2)
            }
    }
    
let parseLiteral bits =
    Literal 6
    
let parseOperator bits =
    Operator []
    
let parseBody header bits =
    match header.TypeId with
    | 4 -> parseLiteral bits
    | _ -> parseOperator bits

let parsePacket (bits: string) =
    let headerResult = parseHeader bits
    
    let body = parseBody headerResult.Element bits[..headerResult.BitsRead]
    
    { Header = headerResult.Element; Body = body }
    
let result = parsePacket bits
result.Dump()