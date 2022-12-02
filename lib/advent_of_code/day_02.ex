defmodule AdventOfCode.Day02 do
  defp parse_opponent(opp) do
    case opp do
      "A" -> :rock
      "B" -> :paper
      "C" -> :scissors
    end
  end

  defp parse_player(opp) do
    case opp do
      "X" -> :rock
      "Y" -> :paper
      "Z" -> :scissors
    end
  end

  defp parse_result(opp) do
    case opp do
      "X" -> :lose
      "Y" -> :draw
      "Z" -> :win
    end
  end

  defp score_shape(shape) do
    case shape do
      :rock -> 1
      :paper -> 2
      :scissors -> 3
    end
  end

  defp play(opp, player) do
    case {opp, player} do
      {:rock, :paper} -> :win
      {:paper, :scissors} -> :win
      {:scissors, :rock} -> :win
      {opp, player} when opp == player -> :draw
      {_, _} -> :lose
    end
  end

  defp score_result(result) do
    case result do
      :win -> 6
      :draw -> 3
      :lose -> 0
    end
  end

  defp find_player(opp, result) do
    Enum.find([:rock, :paper, :scissors], fn player -> play(opp, player) == result end)
  end

  defp simulate_part1(line) do
    [opponent, player] = String.split(line, " ")
    opponent = parse_opponent(opponent)
    player = parse_player(player)
    score_result(play(opponent, player)) + score_shape(player)
  end

  defp simulate_part2(line) do
    [opponent, result] = String.split(line, " ")
    opponent = parse_opponent(opponent)
    result = parse_result(result)
    player = find_player(opponent, result)
    score_result(result) + score_shape(player)
  end

  defp simulate(lines, accum, simulate_single) do
    case lines do
      [] ->
        accum

      [line | rest] ->
        pts = simulate_single.(line)
        simulate(rest, pts + accum, simulate_single)
    end
  end

  def part1(_args) do
    input = String.trim(AdventOfCode.Input.get!(2))
    # input = "A Y\nB X\nC Z"
    lines = String.split(input, "\n")
    simulate(lines, 0, &simulate_part1/1)
  end

  def part2(_args) do
    input = String.trim(AdventOfCode.Input.get!(2))
    # input = "A Y\nB X\nC Z"
    lines = String.split(input, "\n")
    simulate(lines, 0, &simulate_part2/1)
  end
end
