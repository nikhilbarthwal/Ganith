namespace Ganith

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls.Primitives // RangeBase
open Avalonia.Layout
open Avalonia.Media // Brushes
open Avalonia.Themes.Fluent
open ScottPlot.Avalonia // AvaPlot


module Graph =
    type Point = { x: float; y: float }

    [<AbstractClass>]
    type Segment(width: int, size: int, visible: bool, color: ScottPlot.Color) =
        member this.Width = width
        member this.Size = size
        member this.Color = color
        abstract member Points: int -> Point list
        member this.AllPoints(range: int): Point list option =
            if not visible then None else
                [0 .. range - 1] |> List.map this.Points |> List.concat |> Some


[<AbstractClass>]
type Graph(title: string) =
    abstract member Slider: int with get
    abstract member Title: int -> string
    abstract member Segments: Graph.Segment list

    member this.Plot(): unit =
        let p = this.Segments |> List.choose _.AllPoints(this.Slider) |> List.concat
        let xPts, yPts = (p |> List.map _.x) , (p |> List.map _.y)
        let xStart, xEnd = List.min xPts , List.max xPts
        let yStart, yEnd = List.min yPts , List.max yPts

        let buildWindow(): Window =
            let avaPlot = AvaPlot()
            let plot: ScottPlot.Plot = avaPlot.Plot

            // Dark theme. These live on the Plot (not the plottables),
            // so they survive Plot.Clear() and only need to be set once.
            plot.FigureBackground.Color <- ScottPlot.Colors.Black
            plot.DataBackground.Color <- ScottPlot.Colors.Black
            plot.Axes.Color(ScottPlot.Colors.White) // frame, ticks, tick labels
            // plot.Grid.MajorLineColor <- ScottPlot.Color(60uy, 60uy, 60uy, 255uy)
            plot.HideGrid()
            plot.Axes.Title.Label.ForeColor <- ScottPlot.Colors.White

            let render (p: int) = // Redraw everything for a given p.
                plot.Clear()
                for segment in this.Segments do
                    let pts = segment.Points p
                    if not (List.isEmpty pts) then
                        let xs = pts |> List.map _.x |> List.toArray
                        let ys = pts |> List.map _.y |> List.toArray
                        let s = plot.Add.Scatter(xs, ys)
                        s.Color <- segment.Color
                        s.LineWidth <- float32 <| segment.Width
                        s.MarkerSize <- float32 <| segment.Size

                // keep the axes fixed so the view doesn't jump
                plot.Axes.SetLimits(xStart, xEnd, yStart, yEnd)
                plot.Title(this.Title p)
                avaPlot.Refresh()

            // Slider (snaps to whole numbers 0..pMax) plus a "p = N" readout.
            let slider = Slider(Minimum = 0.0,
                                Maximum = float (this.Slider - 1),
                                Value = 0.0,
                                TickFrequency = 1.0,
                                IsSnapToTickEnabled = true,
                                SmallChange = 1.0,
                                LargeChange = 1.0,
                                MinWidth = 360.0,
                                VerticalAlignment = VerticalAlignment.Center)

            let readout = TextBlock(Text = this.Title 0,
                                    Foreground = Brushes.White,
                                    VerticalAlignment = VerticalAlignment.Center,
                                    Margin = Thickness(12.0, 0.0, 0.0, 0.0),
                                    MinWidth = 64.0)

            let update (e: AvaloniaPropertyChangedEventArgs) =
                Log.Info [$"Setting slider value = {slider.Value}"]
                if Object.ReferenceEquals(e.Property, RangeBase.ValueProperty) then
                    let param =
                        let p = int (Math.Floor slider.Value)
                        if p = this.Slider then p - 1 else p
                    readout.Text <- this.Title param ; render param

            slider.PropertyChanged.Add(update)
            let controls =
                StackPanel(Orientation = Orientation.Horizontal,
                           HorizontalAlignment = HorizontalAlignment.Center,
                           Margin = Thickness(12.0))
            controls.Children.Add(slider)
            // controls.Children.Add(readout)

            let root = DockPanel()
            DockPanel.SetDock(controls, Dock.Bottom)
            root.Children.Add(controls)
            root.Children.Add(avaPlot)  // last child fills the remaining space

            let window = Window(Title = title,
                                Width = 900.0,
                                Height = 600.0,
                                Background = Brushes.Black,
                                Content = root)

            // First draw once the control is attached and sized.
            window.Opened.Add(fun _ -> render 0) ; window

        let app () = { new Application() with
                       override this.Initialize() = this.Styles.Add(FluentTheme())
                       override this.OnFrameworkInitializationCompleted() =
                           match this.ApplicationLifetime with
                           | :? IClassicDesktopStyleApplicationLifetime as desktop ->
                               desktop.MainWindow <- buildWindow()
                           | _ -> ()
                           base.OnFrameworkInitializationCompleted() }

        AppBuilder.Configure<Application>(app)
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime([||]) |> ignore
