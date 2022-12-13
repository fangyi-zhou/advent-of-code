defmodule AdventOfCode.Day06 do
  defp all_unique?(stuffs, threshold) do
    MapSet.size(MapSet.new(stuffs)) == threshold
  end

  defp find_unique_start([hd | rest], idx, acc, threshold) do
    if idx < threshold do
      find_unique_start(rest, idx + 1, acc ++ [hd], threshold)
    else
      if all_unique?([hd | acc], threshold) do
        idx
      else
        case acc do
          [_ | acc_rest] ->
            find_unique_start(rest, idx + 1, acc_rest ++ [hd], threshold)
        end
      end
    end
  end

  def part1(_args) do
    input = AdventOfCode.Input.get!(6)
    input = String.trim(input)
    chars = String.to_charlist(input)
    find_unique_start(chars, 1, [], 4)
  end

  def part2(_args) do
    input = AdventOfCode.Input.get!(6)
    input = String.trim(input)
    chars = String.to_charlist(input)
    find_unique_start(chars, 1, [], 14)
  end
end
