defmodule AdventOfCode.Day01 do
  def part1(_args) do
    input = AdventOfCode.Input.get!(1)
    lines = String.split(input, "\n")
    iterate(lines, 0, 0)
  end

  defp iterate(items, curr, maxi) do
    case items do
      [] ->
        maxi

      ["" | rest] ->
        newmaxi = max(curr, maxi)
        iterate(rest, 0, newmaxi)

      [num_s | rest] ->
        {num, _} = Integer.parse(num_s)
        iterate(rest, curr + num, maxi)
    end
  end

  def part2(_args) do
  end
end
