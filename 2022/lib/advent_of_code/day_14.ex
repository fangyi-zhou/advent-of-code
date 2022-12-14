defmodule AdventOfCode.Day14 do
  defp test do
    "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
  end

  defp parse_coord(coord) do
    [x, y] = String.split(coord, ",")
    {x, _} = Integer.parse(x)
    {y, _} = Integer.parse(y)
    {x, y}
  end

  defp clamp(val, mini, maxi) do
    min(max(val, mini), maxi)
  end

  defp make_step({from_x, from_y}, {to_x, to_y}) do
    dx = clamp(to_x - from_x, -1, 1)
    dy = clamp(to_y - from_y, -1, 1)
    {from_x + dx, from_y + dy}
  end

  defp add_rocks(map, _, []), do: map

  defp add_rocks(map, start, [next | rest]) when start == next do
    map = Map.put(map, start, :rock)
    add_rocks(map, start, rest)
  end

  defp add_rocks(map, start, [next | rest]) do
    map = Map.put(map, start, :rock)
    add_rocks(map, make_step(start, next), [next | rest])
  end

  defp parse([], map, max_y), do: {map, max_y}

  defp parse([line | rest], map, max_y) do
    points = String.split(line, [" ", "->"], trim: true)
    points = Enum.map(points, &parse_coord/1)
    max_y = Enum.reduce(points, max_y, fn {_, curr}, acc -> max(curr, acc) end)
    [start | waypoints] = points
    map = add_rocks(map, start, waypoints)
    parse(rest, map, max_y)
  end

  defp drop_sand(_, max_y, {_, y}, _, acc) when y > max_y, do: acc

  defp drop_sand(map, max_y, {x, y}, start, acc) do
    case Map.get(map, {x, y + 1}) do
      nil ->
        drop_sand(map, max_y, {x, y + 1}, start, acc)

      _ ->
        case Map.get(map, {x - 1, y + 1}) do
          nil ->
            drop_sand(map, max_y, {x - 1, y + 1}, start, acc)

          _ ->
            case Map.get(map, {x + 1, y + 1}) do
              nil ->
                drop_sand(map, max_y, {x + 1, y + 1}, start, acc)

              _ ->
                case Map.get(map, {x, y}) do
                  nil ->
                    map = Map.put(map, {x, y}, :sand)
                    # IO.puts "Dropped at #{x}, #{y}"
                    drop_sand(map, max_y, start, start, acc + 1)

                  _ ->
                    acc
                end
            end
        end
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(14)
    lines = String.split(input, "\n", trim: true)
    {map, max_y} = parse(lines, Map.new(), 0)
    drop_sand(map, max_y, {500, 0}, {500, 0}, 0)
  end

  defp add_floor(map, range_x, y) do
    Enum.reduce(range_x, map, fn x, map ->
      Map.put(map, {x, y}, :floor)
    end)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(14)
    lines = String.split(input, "\n", trim: true)
    {map, max_y} = parse(lines, Map.new(), 0)
    map = add_floor(map, 0..1000, max_y + 2)
    drop_sand(map, max_y + 2, {500, 0}, {500, 0}, 0)
  end
end
