defmodule AdventOfCode.Day10 do
  defp go(_, _, [], acc, _), do: Enum.reverse(acc)

  defp go([], cycle, [mark], acc, value) when cycle == mark do
    go([], cycle, [], [value | acc], value)
  end

  defp go(["noop" | instrs], cycle, [mark | marks], acc, value) when cycle == mark do
    # IO.inspect({cycle, acc, value})
    go(instrs, cycle + 1, marks, [value | acc], value)
  end

  defp go(["noop" | instrs], cycle, marks, acc, value) do
    # IO.inspect({cycle, acc, value})
    go(instrs, cycle + 1, marks, acc, value)
  end

  defp go([addx | instrs], cycle, [mark | marks], acc, value) when cycle + 2 >= mark do
    # IO.inspect({cycle, acc, value})
    go([addx | instrs], cycle, marks, [value | acc], value)
  end

  defp go([addx | instrs], cycle, marks, acc, value) do
    # IO.inspect({cycle, acc, value})
    [_, change] = String.split(addx, " ")
    {change, _} = Integer.parse(change)
    go(instrs, cycle + 2, marks, acc, value + change)
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(10)
    lines = String.split(input, "\n", trim: true)
    marks = [20, 60, 100, 140, 180, 220]
    values = go(lines, 0, marks, [], 1)
    Enum.reduce(List.zip([marks, values]), 0, fn {m, v}, acc -> acc + m * v end)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(10)
    lines = String.split(input, "\n", trim: true)
    marks = Enum.to_list(1..240)
    values = go(lines, 0, marks, [], 1)

    output =
      Enum.reduce(List.zip([marks, values]), "", fn {m, v}, acc ->
        m = rem(m, 40)
        output = if abs(m - v - 1) <= 1, do: "#", else: "."
        IO.puts("M#{m} V#{v} #{output}")
        acc <> output
      end)

    IO.puts(Enum.join(["" | Enum.chunk_every(String.to_charlist(output), 40)], "\n"))
  end
end
