defmodule AdventOfCode.Day01 do
  defp iterate(items, curr, accum) do
    case items do
      [] ->
        accum

      ["" | rest] ->
        [_ | new_accum] = Enum.sort([curr | accum], :asc)
        iterate(rest, 0, new_accum)

      [num_s | rest] ->
        {num, _} = Integer.parse(num_s)
        iterate(rest, curr + num, accum)
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(1)
    lines = String.split(input, "\n")
    [ret] = iterate(lines, 0, [0])
    ret
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(1)
    lines = String.split(input, "\n")
    Enum.reduce(iterate(lines, 0, [0, 0, 0]), 0, fn x, y -> x + y end)
  end
end
