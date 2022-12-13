defmodule AdventOfCode.Day09 do
  defp move_step("R"), do: {1, 0}
  defp move_step("U"), do: {0, 1}
  defp move_step("L"), do: {-1, 0}
  defp move_step("D"), do: {0, -1}

  defp update_tail({x_head, y_head}, {x_tail, y_tail})
       when abs(x_head - x_tail) <= 1 and abs(y_head - y_tail) <= 1 do
    # no need to move
    {x_tail, y_tail}
  end

  defp update_tail({x_head, y_head}, {x_tail, y_tail})
       when x_head != x_tail and y_head != y_tail and abs(x_head - x_tail) == 1 and
              abs(y_head - y_tail) == 2 do
    {x_head, div(y_head + y_tail, 2)}
  end

  defp update_tail({x_head, y_head}, {x_tail, y_tail})
       when x_head != x_tail and y_head != y_tail and abs(y_head - y_tail) == 1 and
              abs(x_head - x_tail) == 2 do
    {div(x_head + x_tail, 2), y_head}
  end

  defp update_tail({x_head, y_head}, {x_tail, y_tail}) do
    {div(x_head + x_tail, 2), div(y_head + y_tail, 2)}
  end

  defp update_tails(_, []), do: []

  defp update_tails(prev, [tail | tails]) do
    new_tail = update_tail(prev, tail)
    [new_tail | update_tails(new_tail, tails)]
  end

  defp move(visited, knots, dir, step) do
    visited = MapSet.put(visited, List.last(knots))

    case step do
      0 ->
        {visited, knots}

      _ ->
        {dx, dy} = move_step(dir)
        [{x_head, y_head} | tails] = knots
        head = {x_head + dx, y_head + dy}
        tails = update_tails(head, tails)
        move(visited, [head | tails], dir, step - 1)
    end
  end

  defp go([], visited, _), do: MapSet.size(visited)

  defp go([line | rest], visited, knots) do
    [dir, steps] = String.split(line, " ")
    {steps, _} = Integer.parse(steps)
    {visited, knots} = move(visited, knots, dir, steps)
    go(rest, visited, knots)
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(9)
    lines = String.split(input, "\n", trim: true)
    knots = List.duplicate({0, 0}, 2)
    go(lines, MapSet.new(), knots)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(9)
    lines = String.split(input, "\n", trim: true)
    knots = List.duplicate({0, 0}, 10)
    go(lines, MapSet.new(), knots)
  end
end
