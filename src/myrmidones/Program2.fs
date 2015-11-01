module LSystem

open System
open System.Drawing
open System.Windows.Forms

////ETAP PIERWSZY - Generowanie wyrażenia

//(Rozszerzenie) Gramatyka stochastyczna
//
//let randomizer = new Random()
//
//let random_element (elements: 'a list) =
//    elements.Item(randomizer.Next(elements.Length))
//
//let grammar (symbol: char) =
//    match symbol with
//        | 'E' -> "F"
//        | 'F' -> ["F[+F]F[-F]F"; "F[+F]F"; "F[-F]F"] |> random_element
//        | any -> (string)any

//Pierwszy krok - definujemy gramatykę.
let grammar (symbol: char) =
    match symbol with
        | 'F' -> "FF"
        | 'E' -> "F[+E]F[-E]+E"
        | any -> (string)any

//Trzeci krok - wyprowadzamy funkcję łączącą znaki.
let concatenate(letters: char list) =
    new String(Array.ofList letters)

//Drugi krok - definujemy procedurę generującą słowa.
let rec generate_word (productions: char -> string ) (word: string) (order: int) (maxOrder: int) =
    match order with
        | x when x < maxOrder ->
            match (List.ofSeq word) with
                | [] -> String.Empty
                | head::tail ->
                    [
                        generate_word productions (productions head) (x+1) maxOrder;
                        generate_word productions (concatenate tail) x maxOrder
                    ]
                    |> (String.Empty |> String.concat)
        | _ -> word


// IV - testujemy generowanie gramatyki
printfn "%s" (generate_word grammar "E" 0 3)



////ETAP TRZECI - Rysowanie LSystemu.
let negate = fun x -> -x
let multiply = fun x y -> x * y

let degrees_to_radians (angle: float) : float =
    angle * Math.PI / 180.0

let calculateEnd (start: Point) (length: float) (angle: float) : Point =
    let radians = degrees_to_radians angle
    let x_shift = sin(radians)*length
    //let y_shift = cos(radians)*length * -1.0
    //let y_shift = negate(cos(radians)*length) //pierwsza wersja
    //let y_shift = (radians |> cos) * length |> negate //druga wersja
    let y_shift = radians |> cos |> multiply <| length |> negate
    new Point(start.X + (int)x_shift, start.Y + (int)y_shift)


let drawLine (graphics: Graphics) (pen: Pen) (start: Point) (length: float) (angle: float) =
    let end_point = calculateEnd start length angle
    graphics.DrawLine(pen, start, end_point)
    end_point


////ETAP DRUGI - Rysujemy okienko (przykład integracji z pozostałą częscią .NETa).
let fst_3 (a,_,_) = a
let snd_3 (_,b,_) = b
let thr_3 (_,_,c) = c

let rec drawLSystem graphics (pen: Pen) lsystem start length angle (stack: (Point * float * Pen) list) =
    let angle_diff = 20.0
    match (List.ofSeq lsystem) with
        | head::tail ->
            let new_state = match head with
                | '[' -> (start, angle, new Pen(Color.WhiteSmoke, pen.Width*0.7f))
                | ']' -> stack.Head
                | '+' -> (start, angle + angle_diff, pen)
                | '-' -> (start, angle - angle_diff, pen)
                | 'F' ->
                    let end_point = drawLine graphics pen start length angle
                    (end_point, angle, pen)
                | _ -> (start, angle, pen)

            let new_stack = match head with
                | '[' -> new_state::stack
                | ']' -> stack.Tail
                | _ -> stack

            let new_tail = concatenate tail

            drawLSystem graphics (thr_3 new_state) new_tail (fst_3 new_state) length (snd_3 new_state) new_stack
        | _ -> ()

let drawTree graphics width height =
    let start = new Point(width/2, height-50)
    let pen = new Pen(Color.WhiteSmoke, (single)8)
    //let length = 15.0
    let length = 6.0
    let angle = 0.0
    let lsystem = generate_word grammar "E" 0 6
    drawLSystem graphics pen lsystem start length angle []
    //drawLine graphics pen start length angle |> ignore
    ()

//[<EntryPoint>]
let main argv =
    let window_width, window_height = 800, 800
    let form = new Form(Width = window_width, Height = window_height)
    
    //Przykład ustawiania wartości mutowalnych
    form.FormBorderStyle <- FormBorderStyle.FixedDialog
    form.MaximizeBox <- false

    let pictureBox = new PictureBox(Width = window_width, Height = window_height)
    pictureBox.BackColor <- Color.Black
    pictureBox.Dock <- DockStyle.Fill

    let bitmap = new Bitmap(pictureBox.Width, pictureBox.Height)
    let graphics = Graphics.FromImage(bitmap)
    graphics.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.HighQuality

    pictureBox.Image <- bitmap
    form.Controls.Add(pictureBox)

    //Przykład deklaracji funkcji anonimowej.
    form.KeyDown.Add(fun evArgs -> 
    match evArgs.KeyCode with 
        | Keys.Escape -> form.Close()
        | _ -> ()
    )
   
    drawTree graphics window_width window_height

    //System.Windows.Forms.Application.Run(form)
    form.ShowDialog() |> ignore
    0

        
main [||] |> ignore
    