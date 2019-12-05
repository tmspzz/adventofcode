require File
require IO

defmodule Main do
    # Part 1
    def main(filePath \\ "input.txt") do
        case File.read filePath do
            {:ok, contents} ->
                    [firstWirePath, secondWirePath] = contents 
                        |> String.split("\n", trim: true)
                        |> fn l -> Enum.map(l, fn i -> String.split(i, ",", trim: true) end) end.()
                        |> fn l -> Enum.map(l, fn i -> Enum.map(i, &WirePath.fromString/1) end) end.()
                    
                    [firstWireSegments, secondWireSegments] = 
                        [firstWirePath, secondWirePath] |> Enum.map(&wirePathsToSegments/1)

                    intersections = findSegmentsIntersections(firstWireSegments, secondWireSegments) 
                        |> Enum.drop(1) # wires always Intersec at the origin, do drop it
                    
                    intersections
                        |> Enum.map(fn t -> {t , taxyCabDistanceFromOrigin(t)} end)
                        |> Enum.min_by(fn {_, distance} -> distance end)

                    lengthsToIntersections = intersections |> Enum.map(fn i -> 
                        # If a wire visits a position on the grid multiple times, use the steps value from the first time it visits that position
                        w1PathWithOutEndSegment = Enum.take_while(firstWireSegments, fn s -> not crossesPoint(i, s) end) 
                        w1PathLastSegment = Enum.drop(firstWireSegments, Kernel.length(w1PathWithOutEndSegment)) |> Kernel.hd
                        w1LastSteps = stepsForSegmentToPoint(w1PathLastSegment, i)
                        w2PathWithOutEndSegment = Enum.take_while(secondWireSegments, fn s -> not crossesPoint(i, s) end)
                        w2PathLastSegment = Enum.drop(secondWireSegments, Kernel.length(w2PathWithOutEndSegment)) |> Kernel.hd
                        w2LastSteps = stepsForSegmentToPoint(w2PathLastSegment, i)
                        {i, { (w1PathWithOutEndSegment |> (Enum.map(&stepsForSegment/1) |> Enum.sum)) + w1LastSteps, w1PathWithOutEndSegment }, { (w2PathWithOutEndSegment |> (Enum.map(&stepsForSegment/1) |> Enum.sum)) + w2LastSteps, w2PathWithOutEndSegment }} end)

                    lengthsToIntersections
                        |> Enum.min_by(fn {_, {l1, _}, {l2, _}} -> l1 + l2 end) 
                        |> fn {_, {l1, _}, {l2, _}} -> l1 + l2 end.()
                
            {:error, reason} ->
                IO.puts("Cannot open input.txt: #{reason}")
        end
    end

    def taxyCabDistanceFromOrigin({x, y}) do
        Kernel.abs(x) + Kernel.abs(y)
    end

    def findSegmentsIntersections(firstWireSegments, secondWireSegments) do

        allIntersections = 
            (for segment1 <- firstWireSegments, segment2 <- secondWireSegments,  into: [], do: intersections(segment1, segment2)) 
                |> Enum.filter(fn x -> not (Enum.empty? x) end)
        Enum.concat(allIntersections)
    end

    def test() do
        IO.inspect(main("example.txt"))
        IO.inspect(main("test1.txt"))
        IO.inspect(main("test2.txt"))
    end


    def wirePathsToSegments(wirePaths) do    
        origin = {0, 0}
        [head | tail] = wirePaths
        accumutalor = [{ origin, applyWirePathDisplacement(origin, head) }]

        Enum.reduce(tail, accumutalor, fn element, acc -> 
            {_, endOfSegment} = List.last(acc)
            acc ++ [{endOfSegment, applyWirePathDisplacement(endOfSegment, element) }] end
        )
    end

    def applyWirePathDisplacement(point, wirePath) do
        {x, y} = point
        case wirePath.direction do
            :R -> { x + wirePath.amount, y }
            :L -> { x - wirePath.amount, y }
            :U -> { x, y + wirePath.amount }
            :D -> { x, y - wirePath.amount }
        end
    end

    def intersections(segment1, segment2) do
        { { s1StartX, s1StartY } , { s1EndX, s1EndY} } = segment1
        { { s2StartX, s2StartY } , { s2EndX, s2EndY} } = segment2

        s1XSet = Enum.into(s1StartX..s1EndX, MapSet.new)
        s2XSet = Enum.into(s2StartX..s2EndX, MapSet.new)

        s1YSet = Enum.into(s1StartY..s1EndY, MapSet.new) 
        s2YSet = Enum.into(s2StartY..s2EndY, MapSet.new) 

        xIntersections = MapSet.intersection(s1XSet, s2XSet) |> MapSet.to_list
        yIntersections = MapSet.intersection(s1YSet, s2YSet) |> MapSet.to_list

        cond do
            Enum.empty?(xIntersections) or Enum.empty?(yIntersections) -> []
            Enum.count(xIntersections) > 1 -> Enum.into(xIntersections, [], fn x -> {x, List.first(yIntersections) } end)
            Enum.count(yIntersections) > 1 -> Enum.into(yIntersections, [], fn y -> {List.first(xIntersections), y} end)
            Enum.count(xIntersections) == 1 and Enum.count(yIntersections) == 1 -> [{List.first(xIntersections), List.first(yIntersections)}]
        end
    end

    def crossesPoint({x, y}, { {p1x, p1y}, {p2x, p2y} }) do

        crossesX = Enum.member?(p1x..p2x, x) 
        crossesY = Enum.member?(p1y..p2y, y)

        crossesX and crossesY
    end

    def stepsForSegment({{p1x, p1y}, {p2x, p2y}}) do
        Kernel.abs(p1x-p2x) + Kernel.abs(p1y-p2y)
    end

    def stepsForSegmentToPoint({{p1x, p1y}, _}, {x, y}) do
        Kernel.abs(p1x-x) + Kernel.abs(p1y-y)
    end
end


defmodule WirePath do
    defstruct direction: :R, amount: 0 

    def fromString(string) do
        << d :: utf8, rest:: binary>> = string
        %WirePath { direction: List.to_string([d]) |> String.to_atom, amount: List.to_string([rest]) |> String.to_integer }
    end
end