defmodule AdventOfCode.Day03 do
  defp make_set(str) do
    Enum.reduce(String.to_charlist(str), MapSet.new(), fn ch, set -> MapSet.put(set, ch) end)
  end

  defp find_common(stuffs) do
    sets = Enum.map(stuffs, &make_set/1)
    diff = Enum.reduce(sets, &MapSet.intersection/2)
    [elem] = MapSet.to_list(diff)
    elem
  end

  defp find_prio(line) do
    half = div(String.length(line), 2)
    first = String.slice(line, 0, half)
    second = String.slice(line, half, half)
    find_common([first, second])
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
    input = AdventOfCode.Input.get!(3)
    input = String.trim(input)
    lines = String.split(input, "\n")
    groups = Enum.chunk_every(lines, 3)
    Enum.reduce(groups, 0, fn g, acc -> acc + score_prio(find_common(g)) end)
  end
end
