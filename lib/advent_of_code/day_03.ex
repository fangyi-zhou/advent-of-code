defmodule AdventOfCode.Day03 do
  defp make_set(str) do
    Enum.reduce(String.to_charlist(str), MapSet.new(), fn ch, set -> MapSet.put(set, ch) end)
  end

  defp find_prio(line) do
    half = div(String.length(line), 2)
    first = String.slice(line, 0, half)
    second = String.slice(line, half, half)
    first_set = make_set(first)
    second_set = make_set(second)
    diff = MapSet.intersection(first_set, second_set)
    [elem] = MapSet.to_list(diff)
    elem
  end

  defp score_prio(p) do
    case p do
      p when p >= ?a and p <= ?z -> p - ?a + 1
      p when p >= ?A and p <= ?Z -> p - ?A + 27
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(3)
    input = String.trim(input)
    lines = String.split(input, "\n")
    prios = Enum.map(lines, &find_prio/1)
    Enum.reduce(prios, 0, fn p, acc -> acc + score_prio(p) end)
  end

  def part2(_args) do
  end
end
