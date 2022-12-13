defmodule AdventOfCode.Day01 do
  defp iterate([], _, accum) do
    accum
  end

  defp iterate(["" | rest], curr, accum) do
    [_ | new_accum] = Enum.sort([curr | accum], :asc)
    iterate(rest, 0, new_accum)
  end

  defp iterate([num_s | rest], curr, accum) do
    {num, _} = Integer.parse(num_s)
    iterate(rest, curr + num, accum)
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(1)
    lines = String.split(input, "\n")
    Enum.sum(iterate(lines, 0, [0]))
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(1)
    lines = String.split(input, "\n")
    Enum.sum(iterate(lines, 0, [0, 0, 0]))
  end
end
