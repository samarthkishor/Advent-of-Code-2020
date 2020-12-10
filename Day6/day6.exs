defmodule Day6 do
  @moduledoc """
  Computes the answer for AoC Day 6
  """

  def part1 do
    File.stream!("input.txt")
    |> Stream.chunk_by(fn str -> str == "\n" end)
    |> Stream.filter(fn s -> s != ["\n"] end)
    |> Stream.map(fn group -> Stream.map(group, &String.trim/1) end)
    |> Stream.map(fn group -> Stream.flat_map(group, &String.split(&1, "", trim: true)) end)
    |> Stream.map(&MapSet.new/1)
    |> Stream.map(&MapSet.size/1)
    |> Enum.sum()
  end

  def part2 do
    File.stream!("input.txt")
    |> Stream.chunk_by(fn str -> str == "\n" end)
    |> Stream.filter(fn s -> s != ["\n"] end)
    |> Stream.map(fn group -> Stream.map(group, &String.trim/1) end)
    |> Stream.map(fn group -> Stream.map(group, &String.split(&1, "", trim: true)) end)
    |> Stream.map(fn group ->
      group
      |> Enum.map(&MapSet.new/1)
      |> Enum.reduce(fn ans, acc -> MapSet.intersection(ans, acc) end)
    end)
    |> Stream.map(&MapSet.size/1)
    |> Enum.sum()
  end
end

IO.puts("Part 1: #{Day6.part1()}")
IO.puts("Part 2: #{Day6.part2()}")
