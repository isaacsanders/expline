defmodule Expline.Spline do
  alias Expline.Matrix
  alias Expline.Vector

  @moduledoc """
  `Expline.Spline` is the module defining the struct and functions for
  constructing cubic splines and interpolating values along them. It is the core
  module and data structure for the library.

  ## Mathematics

  Expline uses information from the
  ["Spline interpolation"](https://en.wikipedia.org/wiki/Spline_interpolation#Algorithm_to_find_the_interpolating_cubic_spline),
  ["Cholesky decomposition"](https://en.wikipedia.org/wiki/Cholesky_decomposition#The_Cholesky.E2.80.93Banachiewicz_and_Cholesky.E2.80.93Crout_algorithms),
  and ["Triangular matrix"](https://en.wikipedia.org/wiki/Triangular_matrix#Forward_and_back_substitution)
  Wikipedia pages.

  Spline interpolation requires the curvatures at the spline's given points.
  To determine the curvatures, a system of linear equations needs to be solved.
  The matrix `X` in the system `Xk = y` (where the solution `k` is the vector of
  curvatures for the spline) is by definition Hermitian and positive-definite,
  a conclusion one can make by analyzing the equations and enforcing certain
  conditions by manipulating the input parameters. This allows us to generate
  the components necessary for solving for the curvatures in a more specialized
  fashion, and in the author's experience, a fairly simple fashion.

  Cholesky decomposition is a special type of Matrix decomposition that applies
  to Hermitian, positive-definite matrices. Its runtime is half of LU
  decomposition, another popular matrix decomposition.

  With the Cholesky decomposition, the procedure described
  in the first paragraph of [the "Applications" section](https://en.wikipedia.org/wiki/Cholesky_decomposition#Applications)
  of the "Cholesky decomposition" Wikipedia page is used to find the
  curvatures.

  ## Performance

  Given `n` points, the algorithm for building a spline is `O(n^3)`.
  """

  @typedoc "The Expline's internal representation of a cubic spline"
  @opaque t :: %__MODULE__{ min: float(),
                            max: float(),
                            ranges: :ordsets.ordset({float(), float()}),
                            points: %{ required(float()) => float() },
                            derivatives: %{ required(float()) => float() } }
  defstruct min: 0.0,
            max: 0.0,
            ranges: :ordsets.new(),
            points: Map.new(),
            derivatives: Map.new()

  @spec from_points(list(Expline.point())) :: __MODULE__.t
  def from_points(list_of_points) do
    points = Map.new(list_of_points)

    xs = points
          |> Map.keys

    min = xs |> Enum.min
    max = xs |> Enum.max
    ranges = xs
              |> Enum.sort
              |> Enum.chunk(2, 1)
              |> Enum.map(fn ([x1, x2]) -> {x1, x2} end)
              |> :ordsets.from_list

    derivatives = make_derivatives(points)

    %__MODULE__{ min: min,
                 max: max,
                 ranges: ranges,
                 points: points,
                 derivatives: derivatives }
  end

  @spec interpolate(__MODULE__.t(), float()) :: {:ok, float()}
  def interpolate(%__MODULE__{} = spline, x) when is_float(x) do
    y = case :ordsets.filter(fn ({x1, x2}) -> x1 <= x and x <= x2 end, spline.ranges) do
      [{x1, x2}] ->
        y1 = Map.get(spline.points, x1)
        y2 = Map.get(spline.points, x2)

        k1 = Map.get(spline.derivatives, x1)
        k2 = Map.get(spline.derivatives, x2)

        # Described by equations (1), (2), (3), and (4) on
        # https://en.wikipedia.org/wiki/Spline_interpolation
        t = (x - x1) / (x2 - x1)
        a = k1 * (x2 - x1) - (y2 - y1)
        b = -k2 * (x2 - x1) + (y2 - y1)

        (1 - t) * y1 + t * y2 + t * (1 - t) * (a * (1 - t) + b * t)
      [] ->
        cond do
          spline.min > x ->
            min_curvature = Map.get(spline.derivatives, spline.min)
            min_y = Map.get(spline.points, spline.min)
            (spline.min - x) * min_curvature + min_y
          spline.max < x ->
            max_curvature = Map.get(spline.derivatives, spline.max)
            max_y = Map.get(spline.points, spline.max)
            (x - spline.max) * max_curvature + max_y
        end
    end
    {:ok, y}
  end

  @spec make_derivatives(%{ required(float()) => float() }) :: %{ required(float()) => float() }
  defp make_derivatives(points) do
    n = map_size(points) - 1

    xs = points
          |> Map.keys
          |> Enum.sort

    [x0, x1] = Enum.take(xs, 2)
    [y0, y1] = Enum.take(xs, 2) |> Enum.map(&(Map.get(points, &1)))

    [xn_1, xn] = Enum.drop(xs, n - 1)
    [yn_1, yn] = Enum.drop(xs, n - 1) |> Enum.map(&(Map.get(points, &1)))

    # Described by equations (15), (16), and (17) on
    # https://en.wikipedia.org/wiki/Spline_interpolation
    system_of_eqns = Expline.Matrix.construct(n + 1, n + 2, fn
       # first row
       (0, 0) -> 2 / (x1 - x0)
       (0, 1) -> 1 / (x1 - x0)
       (0, j) when j == n + 1 -> 3 * ((y1 - y0) / :math.pow(x1 - x0, 2))

       # last row
       (^n, j) when j == n - 1 -> 1 / (xn - xn_1)
       (^n, ^n) -> 2 / (xn - xn_1)
       (^n, j) when j == n + 1 -> 3 * ((yn - yn_1) / :math.pow(xn - xn_1, 2))

       # middle rows
       (i, j) when j == i - 1 ->
         [xi_1, xi] = -1..0
                      |> Enum.map(fn (offset) -> Enum.at(xs, i + offset) end)
         1 / (xi - xi_1)
       (i, i) ->
         [xi_1, xi, xi1] = -1..1
                            |> Enum.map(fn (offset) -> Enum.at(xs, i + offset) end)
         2 * ((1 / (xi - xi_1)) + (1 / (xi1 - xi)))
       (i, j) when j == i + 1 ->
         [xi, xi1] = 0..1
                      |> Enum.map(fn (offset) -> Enum.at(xs, i + offset) end)
         1 / (xi1 - xi)
       (i, j) when j == n + 1 ->
         [xi_1, xi, xi1] = -1..1
                            |> Enum.map(fn (offset) -> Enum.at(xs, i + offset) end)
         [yi_1, yi, yi1] =  [xi_1, xi, xi1]
                            |> Enum.map(&(Map.get(points, &1)))
         3 * (
           ((yi - yi_1) / :math.pow(xi - xi_1, 2)) +
           ((yi1 - yi) / :math.pow(xi1 - xi, 2))
         )

       # empty terms
       (_i, _j) -> 0.0
    end)

    {:ok, matrix, vector} = Matrix.disaugment(system_of_eqns)
    {:ok, l} = Matrix.cholesky_decomposition(matrix)
    {:ok, y} = Matrix.forward_substitution(l, vector)
    {:ok, derivative_vector} = l |> Matrix.transpose |> Matrix.backward_substitution(y)

    Enum.zip(xs, Vector.to_list(derivative_vector)) |> Map.new
  end
end
