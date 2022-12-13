defmodule AdventOfCode.Day02 do
  defp parse_opponent("A"), do: :rock
  defp parse_opponent("B"), do: :paper
  defp parse_opponent("C"), do: :scissors

  defp parse_player("X"), do: :rock
  defp parse_player("Y"), do: :paper
  defp parse_player("Z"), do: :scissors

  defp parse_result("X"), do: :lose
  defp parse_result("Y"), do: :draw
  defp parse_result("Z"), do: :win

  defp score_shape(:rock), do: 1
  defp score_shape(:paper), do: 2
  defp score_shape(:scissors), do: 3

  defp play(:rock, :paper), do: :win
  defp play(:paper, :scissors), do: :win
  defp play(:scissors, :rock), do: :win
  defp play(opp, player) when opp == player, do: :draw
  defp play(_, _), do: :lose

  defp score_result(:win), do: 6
  defp score_result(:draw), do: 3
  defp score_result(:lose), do: 0

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

  defp simulate([], accum, _), do: accum

  defp simulate([line | rest], accum, simulate_single) do
    pts = simulate_single.(line)
    simulate(rest, pts + accum, simulate_single)
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
