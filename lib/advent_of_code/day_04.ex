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

  defp score_interval_1?({interval1, interval2}) do
    interval_contains?(interval1, interval2) or interval_contains?(interval2, interval1)
  end

  defp score_interval_2?({{start1, finish1}, {start2, finish2}}) do
    start = max(start1, start2)
    finish = min(finish1, finish2)
    start <= finish
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(4)
    lines = String.split(input, "\n", trim: true)
    parsed = Enum.map(lines, &parse/1)
    Enum.count(parsed, &score_interval_1?/1)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(4)
    lines = String.split(input, "\n", trim: true)
    parsed = Enum.map(lines, &parse/1)
    Enum.count(parsed, &score_interval_2?/1)
  end
end
