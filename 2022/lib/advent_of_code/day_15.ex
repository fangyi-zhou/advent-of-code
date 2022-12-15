defmodule AdventOfCode.Day15 do
  defp test do
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  end

  defp parse_line(line) do
    [
      _sensor,
      _at,
      _x,
      x_sensor,
      _y,
      y_sensor,
      _closest,
      _beacon,
      _is,
      __at,
      __x,
      x_beacon,
      __y,
      y_beacon
    ] = String.split(line, [",", "=", " ", ":"], trim: true)

    {x_sensor, _} = Integer.parse(x_sensor)
    {y_sensor, _} = Integer.parse(y_sensor)
    {x_beacon, _} = Integer.parse(x_beacon)
    {y_beacon, _} = Integer.parse(y_beacon)
    sensor = {x_sensor, y_sensor}
    beacon = {x_beacon, y_beacon}
    {sensor, beacon, man_dist(sensor, beacon)}
  end

  # defp add_pair({sensor, beacon}, map) do
  #   map = Map.put(map, sensor, :sensor)
  #   map = Map.put(map, beacon, :beacon)
  #   map
  # end

  defp man_dist({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  # defp actually_fill(map, _, [], _), do: map

  # defp actually_fill(map, _, [{_, curr} | _], dist) when curr > dist, do: map

  # defp actually_fill(map, visited, [{{x, y}, curr} | rest], dist) do
  #   cond do
  #     MapSet.member?(visited, {x, y}) ->
  #       actually_fill(map, visited, rest, dist)

  #     true ->
  #       map =
  #         case Map.get(map, {x, y}) do
  #           nil ->
  #             Map.put(map, {x, y}, :empty)

  #           _ ->
  #             map
  #         end

  #       visited = MapSet.put(visited, {x, y})

  #       dirs = [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]

  #       new_items =
  #         Enum.map(dirs, fn {dx, dy} ->
  #           {{x + dx, y + dy}, curr + 1}
  #         end)

  #       actually_fill(map, visited, rest ++ new_items, dist)
  #   end
  # end

  # defp fill_blank({sensor, beacon}, map) do
  #   dist = man_dist(sensor, beacon)
  #   actually_fill(map, MapSet.new(), [{sensor, 0}], dist)
  # end

  defp go(_, _, _, x, max_x, acc) when x > max_x, do: acc

  defp go(sensors, beacons, y, x, max_x, acc) do
    cond do
      Map.has_key?(sensors, {x, y}) ->
        go(sensors, beacons, y, x + 1, max_x, acc)

      MapSet.member?(beacons, {x, y}) ->
        go(sensors, beacons, y, x + 1, max_x, acc)

      Enum.any?(sensors, fn {sensor, min_d} ->
        d = man_dist(sensor, {x, y})
        min_d >= d
      end) ->
        go(sensors, beacons, y, x + 1, max_x, acc + 1)

      true ->
        go(sensors, beacons, y, x + 1, max_x, acc)
    end
  end

  def part1(_args) do
    # input = test()
    input = AdventOfCode.Input.get!(15)
    lines = String.split(input, "\n", trim: true)
    pairs = Enum.map(lines, &parse_line/1)
    sensors = Map.new(Enum.map(pairs, fn {s, _, d} -> {s, d} end))
    beacons = MapSet.new(Enum.map(pairs, fn {_, b, _} -> b end))
    max_man_dist = Enum.reduce(pairs, 0, fn {_, _, d}, acc -> max(d, acc) end)

    min_x =
      Enum.reduce(pairs, 99_999_999, fn {{x1, _}, {x2, _}, _}, acc -> min(x1, min(x2, acc)) end)

    max_x = Enum.reduce(pairs, 0, fn {{x1, _}, {x2, _}, _}, acc -> max(x1, max(x2, acc)) end)
    # map = Enum.reduce(pairs, Map.new(), &add_pair/2)
    # beacons = Enum.map(Enum.filter(map, fn {_, v} -> v == :beacon end), fn {k, _} -> k end)
    # sensors = Enum.map(Enum.filter(map, fn {_, v} -> v == :sensor end), fn {k, _} -> k end)
    # map = Enum.reduce(pairs, map, &fill_blank/2)
    # length(Enum.filter(map, fn {{_, y}, v} -> y == 2000000 and v == :empty end))
    go(sensors, beacons, 2_000_000, min_x - max_man_dist - 1, max_x + max_man_dist + 1, 0)
  end

  def part2(_args) do
  end
end
