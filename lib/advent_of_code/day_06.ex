defmodule AdventOfCode.Day06 do
  defp all_unique?(stuffs) do
    MapSet.size(MapSet.new(stuffs)) == 4
  end

  defp find_unique_start([hd | rest], idx, acc) do
    if idx < 4 do
      find_unique_start(rest, idx + 1, acc ++ [hd])
    else
      if all_unique?([hd | acc]) do
        idx
      else
        case acc do
          [_ | acc_rest] ->
            find_unique_start(rest, idx + 1, acc_rest ++ [hd])
        end
      end
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(6)
    input = String.trim(input)
    chars = String.to_charlist(input)
    find_unique_start(chars, 1, [])
  end

  def part2(_args) do
  end
end
