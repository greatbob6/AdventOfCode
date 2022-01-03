<Query Kind="FSharpProgram" />

let getData filePrefix =
    let path = Path.Combine(Path.GetDirectoryName(Util.CurrentQueryPath), $"{filePrefix}_input.txt")
    System.IO.File.ReadAllLines(path)

let rawData = 
    getData "puzzle"
    |> Array.map (fun line -> line.ToCharArray())

let rawAlgorithm = rawData[0]
let rawImage = rawData[2..]

let convertRowToBinary = Array.map (fun c -> if c = '#' then 1 else 0)
let convertImageToBinary = Array.map convertRowToBinary
let padRow padNewVal (row: int[]) =
    Array.concat (seq { [| padNewVal; padNewVal |]; row; [| padNewVal; padNewVal |] })
    
let padImage padNewVal (image: int[][]) =
    let topTwoRows = [|
            Array.create ((Array.length image[0]) + 4) padNewVal
            Array.create ((Array.length image[0]) + 4) padNewVal
        |]
        
    let bottomTwoRows = [|
            Array.create ((Array.length image[0]) + 4) padNewVal
            Array.create ((Array.length image[0]) + 4) padNewVal
        |]
        
    let imageData = Array.map (padRow padNewVal) image
    
    Array.concat (seq { topTwoRows; imageData; bottomTwoRows })
    
let algorithmValueFromPixel (algorithm: int[]) (image: int[][]) (x, y) =
    let idx = Convert.ToInt32($"{image[y-1][x-1]}{image[y-1][x]}{image[y-1][x+1]}{image[y][x-1]}{image[y][x]}{image[y][x+1]}{image[y+1][x-1]}{image[y+1][x]}{image[y+1][x+1]}", 2)
    
    algorithm[idx]
    
let enhanceImage (algorithm: int[]) (image: int[][]) padNewVal =
    let paddedImage = image |> (padImage padNewVal)
    
    let lastYIndex = (Array.length paddedImage) - 1
    let lastXIndex = (Array.length paddedImage[0]) - 1
    
    let enhanced =
        paddedImage
        |> Array.mapi (fun y row ->
            if y = 0 || y = lastYIndex then
                row
            else
                row |> Array.mapi (fun x v ->
                    if x = 0 || x = lastXIndex then
                        v
                    else
                        algorithmValueFromPixel algorithm paddedImage (x, y)
                    )
            )
    enhanced[1..(Array.length enhanced) - 2]
    |> Array.map (fun a -> a[1..(Array.length enhanced[0]) - 2])

let binaryAlgorithm = rawAlgorithm |> convertRowToBinary
let binaryImage = rawImage |> convertImageToBinary

let enhanced = enhanceImage binaryAlgorithm binaryImage 0
let enhanced2 = enhanceImage binaryAlgorithm enhanced (binaryAlgorithm[0])

//(Array2D.map (fun v -> if v = 1 then '#' else '.') (array2D enhanced2)).Dump()

let onCount =
    enhanced2
    |> Array.collect id
    |> Array.fold (+) 0

onCount.Dump()