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

    let pen = new Pen(Color.WhiteSmoke, (single)5)

    let draw = graphics |> drawTree
    let start_point = new Point(window_width/2, window_height-50)

    draw pen start_point

    Application.Run(form);

    0

//let operation (in opposite to mutable definition)
//.NET platform integration
//partial function apply
//functions as data
