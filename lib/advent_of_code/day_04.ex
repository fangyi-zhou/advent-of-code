defmodule AdventOfCode.Day04 do
  defp parse_interval(interval) do
    [start, finish] = String.split(interval, "-")
    {start, _} = Integer.parse(start)
    {finish, _} = Integer.parse(finish)
    {start, finish}
  end

  defp parse(line) do
    [interval1, interval2] = String.split(line, ",")
    {parse_interval(interval1), parse_interval(interval2)}
  end

  defp interval_contains?({start1, finish1}, {start2, finish2}) do
    start1 <= start2 and finish1 >= finish2
  end

  defp score_interval({interval1, interval2}) do
    if interval_contains?(interval1, interval2) or
         interval_contains?(interval2, interval1) do
      1
    else
      0
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(4)
    input = String.trim(input)
    lines = String.split(input, "\n")
    parsed = Enum.map(lines, &parse/1)
    Enum.reduce(parsed, 0, fn line, acc -> score_interval(line) + acc end)
  end

  def part2(_args) do
  end
end
