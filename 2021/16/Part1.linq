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
    getData "puzzle"
    |> Array.head
    |> String.collect hexMap
    |> (fun s -> s.ToCharArray())
    
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
    | Literal of int64
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

let parseHeader (bits: char[]) =
    {
        BitsRead = 6
        Element =
            {
                Version = Convert.ToInt32((String bits[..2]), 2)
                TypeId = Convert.ToInt32((String bits[3..5]), 2)
            }
    }
    
let parseLiteral (bits: char[]) =
    let rec parseNumberBits values startIdx (bits: char[]) =
        let cont = (bits[startIdx] = '1')
        let valueBits = bits[startIdx+1..startIdx+4]
        
        if cont then
            parseNumberBits (valueBits :: values) (startIdx + 5) bits
        else
            (valueBits :: values)
    
    let numbers = parseNumberBits [] 0 bits
    
    let number =
        numbers
        |> List.rev
        |> List.collect Array.toList
        |> List.toArray
        |> String
        |> (fun s -> Convert.ToInt64(s, 2))
    
    { BitsRead = (List.length numbers) * 5; Element = Literal number }
    
let rec parseOperator (bits: char[]) =
    let lengthTypeId = bits[0]
    
    if lengthTypeId = '0' then
        let subPacketBitCount = bits[1..15] |> String |> (fun s -> Convert.ToInt32(s, 2))
        
        let rec packetParser packets charsRemaining pbits =
            let p = parsePacket pbits
            
            let remaining = charsRemaining - p.BitsRead
            
            if remaining > 0 then
                packetParser (p.Element :: packets) remaining pbits[p.BitsRead..]
            else
                (p.Element :: packets)
        
        let packets =
            packetParser [] subPacketBitCount bits[16..subPacketBitCount+15]
            |> List.rev
        
        { BitsRead = subPacketBitCount + 16; Element = Operator packets }
    else
        let subPacketCount = bits[1..11] |> String |> (fun s -> Convert.ToInt32(s, 2))
        
        let rec packetParser packetResults packetsRemaining bits =
            let p = parsePacket bits
            
            if packetsRemaining > 0 then
                packetParser (p :: packetResults) (packetsRemaining - 1) bits[p.BitsRead..]
            else
                (p :: packetResults)
                
        let (count, packets) =
            packetParser [] (subPacketCount - 1) bits[12..]
            |> List.fold (fun (count, packets) packetResult -> (count + packetResult.BitsRead, packetResult.Element :: packets)) (12,[])
                
        { BitsRead = count; Element = Operator packets }
        
and
    parseBody header bits =
        match header.TypeId with
        | 4 -> parseLiteral bits
        | _ -> parseOperator bits

and
    parsePacket (bits: char[]) =
        let headerResult = parseHeader bits
        
        let body = parseBody headerResult.Element bits[headerResult.BitsRead..]
        
        {
            BitsRead = headerResult.BitsRead + body.BitsRead
            Element = { Header = headerResult.Element; Body = body.Element }
        }

let rec versionSummer packet =
    let res =
        match packet.Body with
        | Operator subpackets ->
            subpackets
            |> List.fold (fun total p -> total + versionSummer p) packet.Header.Version
        | _ -> packet.Header.Version
        
    res
    
let parsedPacket = parsePacket bits

let result = versionSummer parsedPacket.Element
result.Dump()

parsedPacket.Element.Dump()
