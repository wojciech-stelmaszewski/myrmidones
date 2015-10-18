open System;
open System.Drawing;
open System.Windows.Forms;

let calculateEnd (start:Point) (length:float) (angle:float) = 
    let x_shift = (angle |> sin) * length
    let y_shift = (angle |> cos) * length * -1.0 //because we are drawing upwards
    start + new Size((int)x_shift, (int)y_shift)
    

let drawLine (graphics:Graphics) (pen:Pen) (start:Point) (length:float) (angle:float) =
    let end_point = calculateEnd start length angle
    graphics.DrawLine(pen, start, end_point)
    end_point

    
let rec drawTreeInner (graphics:Graphics) (pen:Pen) (start:Point) (length:float) (angle:float) =
    match length with
    | l when (l < 20.0) -> ()
    | _ -> let draw = pen |> (graphics |> drawLine)
           let end_point = draw start length angle
           drawTreeInner graphics (new Pen(Color.WhiteSmoke, pen.Width*0.8f)) end_point (length * 0.75) (angle - Math.PI/9.0)
           drawTreeInner graphics (new Pen(Color.WhiteSmoke, pen.Width*0.8f)) end_point (length * 0.6) (angle + Math.PI/16.0)
           drawTreeInner graphics (new Pen(Color.WhiteSmoke, pen.Width*0.8f)) end_point (length * 0.5) (angle + Math.PI/8.0)
           ()

//TODO stochastic drawing
//Gradient branches and more interesting background
//REAL L-Systems

//productions:
// E -> F[+E]F[-E]+E
// F -> FF

//order 7

//angle 20

//let rec drawLSystem (production: string) (order: int) (maxOrder: int) (start:Point) (length:float) (angle:float) =
//    match order with
//        | x when x < maxOrder ->
//            match (List.ofSeq production) with
//                | head::tail ->
//                    match head with
//                    | 'F' -> 
//                        drawLSystem "FF" (x + 1) maxOrder start length angle
//                    | 'E' -> 
//                        //drawLSystem "F[+E]F[-E]+E" (x + 1) maxOrder start length angle
//                        drawLSystem "F[E]E" (x + 1) maxOrder start length angle
//                    //| _ ->  raise (ApplicationException("Unknown symbol!"))
//                    | '[' -> drawLSystem "[" (x + 1) maxOrder start length (angle+20.0)
//                    | ']' -> drawLSystem "]" (x + 1) maxOrder start length (angle-40.0)
//                    drawLSystem (new System.String(tail |> List.toArray)) (x) maxOrder start length angle
//                | [] -> ()
//        | _ -> printf "%s %s %f %f\n" production (start.ToString()) length angle

//drawLSystem "E" 0 3 (new Point(0,0)) 10.0 0.0

let productions (symbol: char) =
    match symbol with
        | 'F' -> "FF"
        //| 'E' -> "F[E]E"
        //| 'E' -> "F+[E]--[E]"
        | 'E' -> "F-[[E]+E]+F[+FE]-E"
        //| 'E' -> "F-[[E]+E]+F[+FE]-E"
        //////| 'E' -> "F[+E]F[-E]+E"
        //| 'E' -> "F[+E]F[-E]+E"
        | char -> (string)char

let rec LSystem (symbol: string) (order: int) (maxOrder: int) =
    match order with
        | x when x < maxOrder ->
            match (List.ofSeq symbol) with
                | head::tail ->
                    [
                        (LSystem (head |> productions) (x+1) maxOrder);
                        (LSystem (String.Concat(tail)) x maxOrder)
                    ] |> (String.Empty |> String.concat)
                |[] -> String.Empty
        | _ -> symbol   

let fst_3 (a, _, _) = a
let snd_3 (_, b, _) = b
let thrd_3 (_, _, c) = c

let rec drawLSystem (graphics:Graphics) (pen:Pen) (lsystem: string) (start: Point) (length: float) (angle: float) (stack: (Point * float * Pen) list) =
    match (List.ofSeq lsystem) with
        | head::tail ->

            //new_start * new_angle

            let angle_diff = 20.0 * Math.PI / 180.0

            let new_conf = match head with
            | '[' -> (start, angle, new Pen(Color.WhiteSmoke, pen.Width*0.8f))
                | ']' -> stack.Head
                | '+' -> (start, angle + angle_diff, pen)
                | '-' -> (start, angle - angle_diff, pen)
                | 'F' ->
                    let end_point = drawLine graphics pen start length angle
                    (end_point, angle, pen)
                | _ -> (start, angle, pen)

            
            let new_stack = match head with
                | '[' -> new_conf :: stack
                | ']' -> stack.Tail
                | _ -> stack

            // first version
//            let new_pen = match head with
//                | '[' -> new Pen(Color.WhiteSmoke, pen.Width*0.8f)
//                | ']' -> thrd_3 stack.Head
//                | _ -> pen
//
//            let new_start = match head with
//                | 'F' -> drawLine graphics pen start length angle
//                | ']' -> fst_3 stack.Head
//                | _ -> start
//
//
//            let new_angle = match head with
//                | '+' -> angle + angle_diff
//                | '-' -> angle - angle_diff
//                | ']' -> snd_3 stack.Head
//                | _ -> angle
//
//
//            let new_stack = match head with
//                | '[' -> (new_start, new_angle, new_pen) :: stack
//                | ']' -> stack.Tail
//                | _ -> stack
//            
            // end of first version

            let new_tail = String.Concat(tail)

            drawLSystem graphics (thrd_3 new_conf) new_tail (fst_3 new_conf) length (snd_3 new_conf) new_stack
        | _ -> ()

//let rec generateLSystem (axiom: string) (order: int) (maxOrder: int)


let rec drawTree (graphics:Graphics) (pen:Pen) (start:Point) =
    let start_length = 200.0
    let start_angle = 0.0
    let draw = pen |> (graphics |> drawLine)
    drawTreeInner graphics pen start start_length start_angle

    ()

[<EntryPoint>]
let main argv =
    let window_width, window_height = 800, 800
    let form = new Form(Width = window_width, Height = window_height)
    form.FormBorderStyle <- FormBorderStyle.FixedDialog
    form.MaximizeBox <- false


    let pictureBox = new PictureBox(Width = window_width, Height = window_height)
    pictureBox.BackColor <- Color.Black;
    pictureBox.Dock <- DockStyle.Fill;

    let bitmap = new Bitmap(pictureBox.Width, pictureBox.Height)
    let graphics = Graphics.FromImage(bitmap)
    graphics.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.HighQuality

    pictureBox.Image <- bitmap;
    form.Controls.Add(pictureBox);

    let pen = new Pen(Color.WhiteSmoke, (single)10)

    let draw = graphics |> drawTree
    let start_point = new Point(window_width/2, window_height-50)

    //draw pen start_point

    drawLSystem graphics pen (LSystem "E" 0 5) start_point 9.0 0.0 []

    Application.Run(form);

    0

//let operation (in opposite to mutable definition)
//.NET platform integration
//partial function apply
//functions as data
