defmodule AdventOfCode.Day16 do
  defp test do
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"
  end

  defp parse_line(line, {flows, connections}) do
    [_value, name | rest] = String.split(line, [" ", "=", ";", ","], trim: true)
    [_has, _flow, _rate, rate | rest] = rest
    {rate, _} = Integer.parse(rate)
    [_tunnel, _lead, _to, _valve | valves] = rest
    flows = Map.put(flows, name, rate)
    connections = Map.put(connections, name, valves)
    {flows, connections}
  end

  defp find_dist_aux(_, _, [{dist, curr} | _], dest) when curr == dest, do: dist

  defp find_dist_aux(connections, visited, [{dist, curr} | rest], dest) do
    if MapSet.member?(visited, curr) do
      find_dist_aux(connections, visited, rest, dest)
    else
      visited = MapSet.put(visited, curr)
      connection = Map.get(connections, curr)

      find_dist_aux(
        connections,
        visited,
        rest ++ Enum.map(connection, fn next -> {dist + 1, next} end),
        dest
      )
    end
  end

  defp find_dist(connections, origin, dest) do
    find_dist_aux(connections, MapSet.new(), [{0, origin}], dest)
  end

  defp go(_, _, valves_to_open, _, _, acc) when map_size(valves_to_open) == 0, do: acc
  defp go(_, _, _, _, time, acc) when time <= 0, do: acc

  defp go(flows, connections, valves_to_open, curr, time, acc) do
    # IO.puts("At #{curr}, time #{time} acc #{acc}")
    # IO.inspect(valves_to_open)

    # {ret, pressure} =
    #   Enum.reduce(valves_to_open, {"", 0}, fn {candidate, flow}, {old_candidate, old_pressure} ->
    #     dist = find_dist(connections, curr, candidate)
    #     pressure = max(0, (time - dist - 1) * flow)
    #     # IO.puts("#{candidate} at #{dist} away, pressure #{pressure}")

    #     if pressure > old_pressure,
    #       do: {{candidate, dist}, pressure},
    #       else: {old_candidate, old_pressure}
    #   end)

    # if pressure == 0 do
    #   acc
    # else
    #   {candidate, dist} = ret
    #   valves_to_open = Map.delete(valves_to_open, candidate)
    #   go(flows, connections, valves_to_open, candidate, time - dist - 1, acc + pressure)
    # end
    Enum.reduce(valves_to_open, 0, fn {candidate, flow}, prev_best ->
      dist = find_dist(connections, curr, candidate)
      pressure = max(0, (time - dist - 1) * flow)
      # IO.puts("#{candidate} at #{dist} away, pressure #{pressure}")
      valves_to_open = Map.delete(valves_to_open, candidate)

      max(
        prev_best,
        go(flows, connections, valves_to_open, candidate, time - dist - 1, acc + pressure)
      )
    end)
  end

  def part1(_args) do
    # input = test
    input = AdventOfCode.Input.get!(16)
    lines = String.split(input, "\n", trim: true)
    {flows, connections} = Enum.reduce(lines, {Map.new(), Map.new()}, &parse_line/2)
    valves_to_open = Map.filter(flows, fn {_, flow} -> flow > 0 end)
    # IO.inspect(valves_to_open)
    go(flows, connections, valves_to_open, "AA", 30, 0)
  end

  def part2(_args) do
  end
end
