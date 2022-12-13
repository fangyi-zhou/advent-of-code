defmodule AdventOfCode.Day13 do
  defp test do
    "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"
  end

  defp compare(x, y) when is_number(x) and is_number(y) do
    cond do
      x < y -> :lt
      x == y -> :eq
      x > y -> :gt
    end
  end

  defp compare(x, y) when is_number(x), do: compare([x], y)
  defp compare(x, y) when is_number(y), do: compare(x, [y])

  defp compare([], []), do: :eq

  defp compare([], _), do: :lt

  defp compare(_, []), do: :gt

  defp compare([hd1 | tl1], [hd2 | tl2]) do
    case compare(hd1, hd2) do
      :eq -> compare(tl1, tl2)
      comp -> comp
    end
  end

  defp in_order?(x, y), do: compare(x, y) == :gt

  defp go([], acc, _), do: acc

  defp go([l1, l2 | rest], acc, idx) do
    {l1, _} = Code.eval_string(l1)
    {l2, _} = Code.eval_string(l2)
    go(rest, if(in_order?(l1, l2), do: acc, else: acc + idx), idx + 1)
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(13)
    lines = String.split(input, "\n", trim: true)
    go(lines, 0, 1)
  end

  defp find(_, [], _, acc), do: acc

  defp find([hd | tl], [hd_to_find | tl_to_find], idx, acc) when hd == hd_to_find do
    find(tl, tl_to_find, idx + 1, acc * idx)
  end

  defp find([_ | tl], to_find, idx, acc) do
    find(tl, to_find, idx + 1, acc)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(13)
    lines = String.split(input, "\n", trim: true)

    lines =
      Enum.map(lines, fn line ->
        {line, _} = Code.eval_string(line)
        line
      end)

    find(
      Enum.sort([[[2]], [[6]] | lines], fn x, y -> compare(x, y) != :gt end),
      [[[2]], [[6]]],
      1,
      1
    )
  end
end
