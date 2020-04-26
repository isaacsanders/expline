defmodule ExplineTest do
  use ExUnit.Case
  use Quixir
  doctest Expline

  @fun_list [&:math.sin/1, &:math.cos/1, &:math.atan/1]
  @too_close_points [{0.0, 0.0}, {-5.0e-324, 0.0}]

  require Expline

  test "a spline can't have points that are too close" do
    ptest points: list(min: 3, of: tuple(like: {float(), float()})) do
      proper_error =
        match?(
          {:error, {:range_too_small, _p1, _p2}},
          Expline.start(@too_close_points ++ points, graceful_shutdown: true)
        )

      assert proper_error
    end
  end

  test "a spline requires 3 points" do
    ptest points: list(max: 2, of: tuple(like: {float(), float()})) do
      proper_error =
        match?({:error, :too_few_points}, Expline.start(points, graceful_shutdown: true))

      assert proper_error
    end
  end

  test "removing points that are too close and ensuring a minimum set of 3 provides a valid spline" do
    ptest points: list(min: 3, of: tuple(like: {float(), float()})) do
      result =
        points
        |> Enum.uniq_by(fn {x, _} -> Float.round(x, 15) end)
        |> Expline.start_link()

      spline =
        case result do
          {:ok, spline} -> spline
          _ -> nil
        end

      assert not is_nil(spline)
    end
  end

  test "splines for various functions" do
    ptest xs: list(min: 100, of: float(min: -50, max: 50)),
          sample_xs: list(min: 10, of: float(min: -10, max: 10)),
          fun: choose(from: Enum.map(@fun_list, &value/1)) do
      points =
        xs
        |> Enum.uniq_by(fn x -> Float.round(x, 15) end)
        |> Enum.map(fn x -> {x, fun.(x)} end)

      spline =
        case Expline.start_link(points) do
          {:ok, spline} -> spline
          _ -> nil
        end

      assert not is_nil(spline)

      errors =
        sample_xs
        |> Enum.map(&Expline.interpolate(spline, &1))
        |> Keyword.new()
        |> Keyword.get_values(:error)

      assert Enum.empty?(errors)
    end
  end
end
