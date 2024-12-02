defmodule UtilsTest do
  use ExUnit.Case
  doctest Utils

  test "placeholder" do
    assert Utils.read_2d_ints("day01")
  end
end
