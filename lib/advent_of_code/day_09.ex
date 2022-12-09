defmodule AdventOfCode.Day09 do
  defp move_step(dir) do
    case dir do
      "R" -> {1, 0}
      "U" -> {0, 1}
      "L" -> {-1, 0}
      "D" -> {0, -1}
    end
  end

  defp update_tail({x_head, y_head}, {x_tail, y_tail}) do
    cond do
      abs(x_head - x_tail) <= 1 and abs(y_head - y_tail) <= 1 ->
        # no need to move
        {x_tail, y_tail}

      x_head != x_tail and y_head != y_tail ->
        cond do
          abs(x_head - x_tail) == 1 and abs(y_head - y_tail) == 2 ->
            {x_head, div(y_head + y_tail, 2)}

          abs(y_head - y_tail) == 1 and abs(x_head - x_tail) == 2 ->
            {div(x_head + x_tail, 2), y_head}

          true ->
            {div(x_head + x_tail, 2), div(y_head + y_tail, 2)}
        end

      true ->
        {div(x_head + x_tail, 2), div(y_head + y_tail, 2)}
    end
  end

  defp update_tails(prev, tails) do
    case tails do
      [] ->
        []

      [tail | tails] ->
        new_tail = update_tail(prev, tail)
        [new_tail | update_tails(new_tail, tails)]
    end
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

  defp go(lines, visited, knots) do
    case lines do
      [] ->
        MapSet.size(visited)

      [line | rest] ->
        [dir, steps] = String.split(line, " ")
        {steps, _} = Integer.parse(steps)
        {visited, knots} = move(visited, knots, dir, steps)
        go(rest, visited, knots)
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(9)
    lines = String.split(input, "\n", trim: true)
    go(lines, MapSet.new(), [{0, 0}, {0, 0}])
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(9)
    lines = String.split(input, "\n", trim: true)

    go(lines, MapSet.new(), [
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0},
      {0, 0}
    ])
  end
end
