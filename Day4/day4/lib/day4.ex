defmodule Day4 do
  @moduledoc """
  Computes the answer for AoC Day 4
  """

  def part1 do
    File.stream!("./lib/input.txt")
    |> Stream.chunk_by(fn str -> str == "\n" end)
    |> Stream.filter(fn passport -> passport != ["\n"] end)
    |> Stream.map(fn passport -> Enum.map(passport, &String.trim/1) end)
    |> Stream.map(&Enum.join(&1, " "))
    |> Stream.map(&String.split(&1, "\s", trim: true))
    |> Stream.map(&create_passport/1)
    |> Stream.map(&valid_passport_1?/1)
    |> Enum.count(fn valid? -> valid? == true end)
  end

  def part2 do
    File.stream!("./lib/input.txt")
    |> Stream.chunk_by(fn str -> str == "\n" end)
    |> Stream.filter(fn passport -> passport != ["\n"] end)
    |> Stream.map(fn passport -> Enum.map(passport, &String.trim/1) end)
    |> Stream.map(&Enum.join(&1, " "))
    |> Stream.map(&String.split(&1, "\s", trim: true))
    |> Stream.map(&create_passport/1)
    |> Stream.map(&valid_passport_2?/1)
    |> Enum.count(fn valid? -> valid? == true end)
  end

  @doc """
  Creates a passport map out of a list of strings
  """
  def create_passport(passport) do
    passport
    |> Stream.map(&String.split(&1, ":"))
    |> Stream.map(fn [key, value] -> {key, value} end)
    |> Map.new()
  end

  @doc """
  Returns true if a passport is valid (has all the right keys)
  """
  def valid_passport_1?(passport) do
    case passport do
      %{
        "byr" => _,
        "iyr" => _,
        "eyr" => _,
        "hgt" => _,
        "hcl" => _,
        "ecl" => _,
        "pid" => _,
        "cid" => _
      } ->
        true

      %{
        "byr" => _,
        "iyr" => _,
        "eyr" => _,
        "hgt" => _,
        "hcl" => _,
        "ecl" => _,
        "pid" => _
      } ->
        true

      _ ->
        false
    end
  end

  def valid_passport_2?(passport) do
    valid_passport_1?(passport) and
      passport
      |> Map.delete("cid")
      |> Map.to_list()
      |> Enum.map(&valid_key?/1)
      |> Enum.reduce(true, fn acc, valid? -> acc and valid? end)
  end

  def valid_key?({"byr", byr}) do
    {n, _} = Integer.parse(byr)
    1920 <= n and n <= 2002 and String.length(byr) == 4
  end

  def valid_key?({"iyr", iyr}) do
    {n, _} = Integer.parse(iyr)
    2010 <= n and n <= 2020 and String.length(iyr) == 4
  end

  def valid_key?({"eyr", eyr}) do
    {n, _} = Integer.parse(eyr)
    2020 <= n and n <= 2030 and String.length(eyr) == 4
  end

  def valid_key?({"hgt", hgt}) do
    case Integer.parse(hgt) do
      {cms, "cm"} -> 150 <= cms and cms <= 193
      {inches, "in"} -> 59 <= inches and inches <= 76
      {_, _} -> false
      :error -> false
    end
  end

  def valid_key?({"hcl", hcl}) do
    String.starts_with?(hcl, "#") and
      String.length(hcl) == 7 and
      hcl
      |> String.slice(1..-1)
      |> String.split("", trim: true)
      |> Enum.map(&String.match?(&1, ~r/[0-9]|[a-f]/))
      |> Enum.reduce(true, fn acc, bool -> acc and bool end)
  end

  def valid_key?({"ecl", ecl}) do
    ecl in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  end

  def valid_key?({"pid", pid}) do
    String.length(pid) == 9 and
      case Integer.parse(pid) do
        {_, _} -> true
        :error -> false
      end
  end
end
