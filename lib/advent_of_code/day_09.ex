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
        if abs(x_head - x_tail) == 1 do
          {x_head, div(y_head + y_tail, 2)}
        else
          {div(x_head + x_tail, 2), y_head}
        end

      true ->
        {div(x_head + x_tail, 2), div(y_head + y_tail, 2)}
    end
  end

  defp move(visited, head, tail, dir, step) do
    visited = MapSet.put(visited, tail)

    case step do
      0 ->
        {visited, head, tail}

      _ ->
        {dx, dy} = move_step(dir)
        {x_head, y_head} = head
        head = {x_head + dx, y_head + dy}
        tail = update_tail(head, tail)
        move(visited, head, tail, dir, step - 1)
    end
  end

  defp go(lines, visited, head, tail) do
    case lines do
      [] ->
        MapSet.size(visited)

      [line | rest] ->
        [dir, steps] = String.split(line, " ")
        {steps, _} = Integer.parse(steps)
        {visited, head, tail} = move(visited, head, tail, dir, steps)
        go(rest, visited, head, tail)
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(9)
    lines = String.split(input, "\n", trim: true)
    go(lines, MapSet.new(), {0, 0}, {0, 0})
  end

  def part2(_args) do
#     input = "R 4
# U 4
# L 3
# D 1
# R 4
# D 1
# L 5
# R 2"
  end
end
