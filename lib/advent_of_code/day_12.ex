defmodule AdventOfCode.Day12 do
  defp test do
    "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
  end

  defp line_to_map([], _, y, acc, start, finish), do: {acc, y, start, finish}

  defp line_to_map([?S | tl], x, y, acc, _, finish) do
    acc = Map.put(acc, {x, y}, 0)
    line_to_map(tl, x, y + 1, acc, {x, y}, finish)
  end

  defp line_to_map([?E | tl], x, y, acc, start, _) do
    acc = Map.put(acc, {x, y}, ?z - ?a)
    line_to_map(tl, x, y + 1, acc, start, {x, y})
  end

  defp line_to_map([height | tl], x, y, acc, start, finish) do
    acc = Map.put(acc, {x, y}, height - ?a)
    line_to_map(tl, x, y + 1, acc, start, finish)
  end

  defp to_map([], x, y, acc, start, finish), do: {acc, x, y, start, finish}

  defp to_map([hd | tl], x, _, acc, start, finish) do
    chars = String.to_charlist(hd)
    {acc, y, start, finish} = line_to_map(chars, x, 0, acc, start, finish)
    to_map(tl, x + 1, y, acc, start, finish)
  end

  defp find(_, _, _, [], _, _), do: 999_999_999_999

  defp find(map, x, y, [{dist, {i, j}} | todos], finish, visited) do
    cond do
      MapSet.member?(visited, {i, j}) ->
        find(map, x, y, todos, finish, visited)

      {i, j} == finish ->
        dist

      true ->
        visited = MapSet.put(visited, {i, j})
        curr = Map.get(map, {i, j})

        moves =
          Enum.filter(
            [{i + 1, j}, {i, j + 1}, {i - 1, j}, {i, j - 1}],
            fn {new_i, new_j} ->
              new_i >= 0 and new_i < x and new_j >= 0 and new_j < y and
                Map.get(map, {new_i, new_j}) <= curr + 1
            end
          )

        find(
          map,
          x,
          y,
          todos ++ Enum.map(moves, fn coord -> {dist + 1, coord} end),
          finish,
          visited
        )
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(12)
    lines = String.split(input, "\n", trim: true)
    {map, x, y, start, finish} = to_map(lines, 0, 0, Map.new(), nil, nil)
    find(map, x, y, [{0, start}], finish, MapSet.new())
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(12)
    # input = test()
    lines = String.split(input, "\n", trim: true)
    {map, x, y, _, finish} = to_map(lines, 0, 0, Map.new(), nil, nil)
    starts = Map.keys(Map.filter(map, fn {_, v} -> v == 0 end))

    Enum.reduce(starts, 9_999_999_999, fn start, acc ->
      min(acc, find(map, x, y, [{0, start}], finish, MapSet.new()))
    end)
  end
end
