require IEx

defmodule Expline.Spline do
  alias Expline.Matrix
  alias Expline.Vector

  @closeness_threshold 1.0e-15

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

  There are a number of ways cubic splines can extrapolate values beyond their
  minimum and maximum. Currently, the only way Expline implements is "Natural
  Spline". Others, such as "Clamped Spline" and "Not-A-Knot", may be
  implemented depending on feedback from users.

  For "Natural Splines", a case of splines where the ends of the spline have no
  curvature, and are therefore can be linearly extrapolated, the matrix `X` in
  the system `Xk = y` (where the solution `k` is the vector of curvatures for
  the spline) is by definition Hermitian and positive-definite, a conclusion one
  can make by analyzing the equations and enforcing certain conditions by
  manipulating the input parameters. This allows us to generate the components
  necessary for solving for the curvatures in a more specialized fashion, and in
  the author's experience, a fairly simple fashion.

  Cholesky decomposition is a special type of Matrix decomposition that applies
  to Hermitian, positive-definite matrices. Its runtime is half of LU
  decomposition, another popular matrix decomposition.

  With the Cholesky decomposition, the procedure described
  in the first paragraph of [the "Applications" section](https://en.wikipedia.org/wiki/Cholesky_decomposition#Applications)
  of the "Cholesky decomposition" Wikipedia page is used to find the
  curvatures.

  ## Performance

  Given `n` points, the algorithm for building a spline is `O(n^3)` and
  interpolating a value is `O(n)`.
  """

  @typedoc "The Expline's internal representation of a cubic spline"
  @opaque t :: %__MODULE__{ min: independent_value(),
                            max: independent_value(),
                            ranges: :ordsets.ordset(range()),
                            points: %{ required(independent_value()) => dependent_value() },
                            derivatives: %{ required(independent_value()) => curvature() } }

  @typedoc """
  The method by which a spline's end conditions are evaluated.

  This can have a significant effect on the runtime for the spline creation.

  "Natural Spline", denoted by the `:natural_spline` extrapolation method, uses
  a particular shape of system of linear equations be solved that is more
  performant to solce than a more general system. Currently, it is also the
  only extrapolation method implemented.
  """
  @type extrapolation_method :: :natural_spline

  @enforce_keys [ :min, :max, :ranges, :points,
                  :derivatives, :extrapolation_method ]
  defstruct [ :min, :max, :ranges, :points,
              :derivatives, :extrapolation_method ]

  @typedoc """
  The type used to denote a value that is independent and may be used to
  interpolate a dependent value from `interpolate/2`, reference a point, or
  determine the extrema of a spline or its ranges.
  """
  @type independent_value() :: float()

  @typedoc """
  The type used to denote a value that depends on another, whether it is by
  relation to an independent value in `t:point/0` or returned by
  `interpolate/2`.
  """
  @type dependent_value() :: float()

  @typedoc """
  The type used to denote the rate of change in the curvature of function at a
  given point.
  """
  @type curvature() :: float()

  @typedoc """
  The type used to initialize a spline and returned from `Expline.interpolate/2`
  and `Expline.interpolate/3`.
  """
  @type point() :: {independent_value(), dependent_value()}

  @typedoc """
  The type used to denote the open range within which a point is interpolated.
  """
  @type range() :: {independent_value(), independent_value()}

  @typedoc """
  The errors that can arise from improper input to `from_points/1`.

  `:too_few_points` occurs when less than 3 points are used to create the
  spline. If this error occurs, providing more points should mitigate the
  issue. If you want to interpolate values between two points,
  [Linear interpolation](https://en.wikipedia.org/wiki/Linear_interpolation)
  may be of more interest, but is not in the interest of this library.

  `{:range_too_small, point(), point()}` occurs when the distance between the
  `t:independent_value/0` in each of at least two points are too close for the
  virtual machine to determine a curvature between them. If this error occurs,
  increasing the resolution between the provided points or dropping points that
  are too close before input are both straightforward examples of mitigation
  strategies.
  """
  @type creation_error() :: :too_few_points
                          | {:range_too_small, point(), point()}


  @typedoc """
  The errors that can arise from improper input to `interpolate/2`.

  `:corrupt_extrema` occurs when the minimum/maximum is not greater/less than a
  point that does not fall into a range in the spline.

  `:corrupt_spline` occurs when the spline's internal information does not
  conform to various mathematical invariants.

  For either of these values, a
  [bug report](https://github.com/isaacsanders/expline/issues)
  may be needed, as in normal operation, neither should occur.
  """
  @type interpolation_error() :: :corrupt_extrema
                               | :corrupt_spline

  @doc """
  Create a spline from a list of floating point pairs (tuples).

  This function bootstraps a spline and prepares it for use in the
  `interpolate/2` function.

  If fewer than 3 points are passed to the function, the function will
  short-circuit and return `{:error, :too_few_points}`. This is a mathematical
  constraint. A cubic spline cannot be built on fewer than 3 points.

  Due to the constraints of Erlang and Elixir, there is a limit on how close
  points can be. If points are too close to each other, this leads to a
  situation where an error similar to dividing by zero occurs. In light of
  this, there is a check that occurs and short-circuits the construction of the
  spline. It returns `{:error, {:range_too_small, p1, p2}}` where `p1` and `p2`
  are two `t:point/0`s that are too close together. If this error is
  encountered, mitigating it is use-case specific. In order to find the points
  that are too close, the following snippet may prove useful:

  ```
  points
  |> Enum.group_by(fn ({x, y}) -> Float.round(x, 15) end)
  ```

  `Float.round/2` has a maximum precision of 15, and while floating point
  numbers appear to have higher precision, with representations with much
  higher points of precision, being able to reliably reconcile points is
  important.

  If this is an issue, a smaller closeness threshold will be used, but until
  then, the threshold for determining points that are too close is 1.0e-15, the
  smallest value that can't be rounded by `Float.round/2`.

  ## Examples

      # A point that is too close
      iex> Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {5.0e-324, 0.0}])
      {:error, {:range_too_small, {0.0, 0.0}, {5.0e-324, 0.0}}}

      # Not enough points
      iex> Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}])
      {:error, :too_few_points}

      # Well-spaced, enough points
      iex> Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}])
      {:ok, %Expline.Spline{derivatives: %{0.0 => 0.9999999999999999,
                                           1.0 => 0.9999999999999999,
                                           2.0 => 1.0000000000000002},
                            extrapolation_method: :natural_spline,
                            max: 2.0, min: 0.0,
                            points: %{0.0 => 0.0, 1.0 => 1.0, 2.0 => 2.0},
                            ranges: [{0.0, 1.0}, {1.0, 2.0}]}}
  """

  @spec from_points(list(point())) :: {:ok, t()}
                                    | {:error, creation_error()}
  def from_points(list_of_points) when length(list_of_points) >= 3 do
    points = Map.new(list_of_points)

    xs = points
          |> Map.keys

    min = xs |> Enum.min
    max = xs |> Enum.max
    ranges = make_ranges(xs)

    case Enum.find(ranges, &range_too_small?/1) do
      {x1, x2} ->
        y1 = Map.get(points, x1)
        y2 = Map.get(points, x2)
        {:error, {:range_too_small, {x1, y1}, {x2, y2}}}
      nil ->
        derivatives = make_derivatives(points)

        spline = %__MODULE__{ min: min,
                              max: max,
                              ranges: :ordsets.from_list(ranges),
                              points: points,
                              derivatives: derivatives,
                              extrapolation_method: :natural_spline }
        {:ok, spline}
    end
  end
  def from_points(list_of_points) when length(list_of_points) < 3,
  do: {:error, :too_few_points}

  @spec make_ranges(list(independent_value())) :: list(range())
  defp make_ranges(xs) do
    xs
    |> Enum.sort
    |> Enum.chunk(2, 1)
    |> Enum.map(fn ([x1, x2]) -> {x1, x2} end)
  end

  @spec range_too_small?(range()) :: boolean()
  defp range_too_small?({x1, x2}) do
    abs(x1 - x2) <= @closeness_threshold
  end

  @doc """
  Interpolate a value from the spline.

  In regular usage, when the function is given a float and a spline, it will
  return a tuple of `{:ok, float}` corresponding to the interpolated value of
  dependent variable from the given value of the independent variable and the
  spline.

  If any of the invariants of the spline's internal representation are not
  satisfied, then `{:error, :corrupt_spline}` will be returned. If this
  happens, please report it, as that would be a sign that there is an issue with
  the underlying data structures or algorithms used in the library.

  ## Examples

      iex> with {:ok, spline} <- Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}]),
      ...> do: Expline.Spline.interpolate(spline, -0.5)
      {:ok, -0.49999999999999994}

      iex> with {:ok, spline} <- Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}]),
      ...> do: Expline.Spline.interpolate(spline, 0.5)
      {:ok, 0.5}

      iex> with {:ok, spline} <- Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}]),
      ...> do: Expline.Spline.interpolate(spline, 1.5)
      {:ok, 1.5}

      iex> with {:ok, spline} <- Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}]),
      ...> do: Expline.Spline.interpolate(spline, 2.5)
      {:ok, 2.5}
  """
  @spec interpolate(t(), independent_value()) :: {:ok, dependent_value()}
                                               | {:error, interpolation_error()}
                                               | {:error, :corrupt_spline}
  def interpolate(%__MODULE__{} = spline, x) when is_float(x) do
    with :error <- Map.fetch(spline.points, x) do
      case :ordsets.filter(fn ({x1, x2}) -> x1 < x and x < x2 end, spline.ranges) do
        [{_x1, _x2} = range] ->
          do_interpolate(spline, range, x)
        [] ->
          extrapolate(spline, x)
        _ranges ->
          {:error, :corrupt_spline}
      end
    end
  end

  @spec do_interpolate(t(), range(), independent_value()) :: {:ok, dependent_value()}
  defp do_interpolate(%__MODULE__{} = spline, {x1, x2}, x) do
    y1 = Map.get(spline.points, x1)
    y2 = Map.get(spline.points, x2)

    k1 = Map.get(spline.derivatives, x1)
    k2 = Map.get(spline.derivatives, x2)

    # Described by equations (1), (2), (3), and (4) on
    # https://en.wikipedia.org/wiki/Spline_interpolation
    t = (x - x1) / (x2 - x1)
    a = k1 * (x2 - x1) - (y2 - y1)
    b = -k2 * (x2 - x1) + (y2 - y1)

    y = (1 - t) * y1 + t * y2 + t * (1 - t) * (a * (1 - t) + b * t)
    {:ok, y}
  end

  @spec extrapolate(t(), independent_value()) :: {:ok, dependent_value()}
                                               | {:error, :corrupt_extrema}
  defp extrapolate(spline, x) do
    cond do
      spline.min > x ->
        min_curvature = Map.get(spline.derivatives, spline.min)
        min_y = Map.get(spline.points, spline.min)
        y = (x - spline.min) * min_curvature + min_y
        {:ok, y}
      spline.max < x ->
        max_curvature = Map.get(spline.derivatives, spline.max)
        max_y = Map.get(spline.points, spline.max)
        y = (x - spline.max) * max_curvature + max_y
        {:ok, y}
      true -> {:error, :corrupt_extrema}
    end
  end

  @doc """
  Interpolate a curvature from the spline.

  ## Examples

      iex> with {:ok, spline} <- Expline.Spline.from_points([{0.0, 0.0}, {1.0, 1.0}, {2.0, 2.0}]),
      ...> do: Expline.Spline.interpolate_curvature(spline, 0.5)
      {:ok, 1.0}
  """
  @spec interpolate_curvature(t(), independent_value()) :: {:ok, curvature()}
                                                         | {:error, interpolation_error()}
                                                         | {:error, :corrupt_spline}
  def interpolate_curvature(%__MODULE__{} = spline, x) when is_float(x) do
    with :error <- Map.fetch(spline.derivatives, x) do
      case :ordsets.filter(fn ({x1, x2}) -> x1 < x and x < x2 end, spline.ranges) do
        [{_x1, _x2} = range] ->
          do_interpolate_curvature(spline, range, x)
        [] ->
          extrapolate_curvature(spline, x)
        _ranges ->
          {:error, :corrupt_spline}
      end
    end
  end

  @spec do_interpolate_curvature(t(), range(), independent_value()) :: {:ok, dependent_value()}
  defp do_interpolate_curvature(%__MODULE__{} = spline, {x1, x2}, x) do
    y1 = Map.get(spline.points, x1)
    y2 = Map.get(spline.points, x2)

    k1 = Map.get(spline.derivatives, x1)
    k2 = Map.get(spline.derivatives, x2)

    # Described by equations (1), (2), (3), and (4) on
    # https://en.wikipedia.org/wiki/Spline_interpolation
    t = (x - x1) / (x2 - x1)
    a = k1 * (x2 - x1) - (y2 - y1)
    b = -k2 * (x2 - x1) + (y2 - y1)

    dy = ((y2 - y1) + (1 - 2 * t) * (a * (1 - t) + b * t) + t * (1 - t) * (b - a)) / (x2 - x1)
    {:ok, dy}
  end

  @spec extrapolate_curvature(t(), independent_value()) :: {:ok, dependent_value()}
                                                         | {:error, :corrupt_extrema}
  defp extrapolate_curvature(spline, x) do
    cond do
      spline.min > x ->
        {:ok, Map.get(spline.derivatives, spline.min)}
      spline.max < x ->
        {:ok, Map.get(spline.derivatives, spline.max)}
      true -> {:error, :corrupt_extrema}
    end
  end

  @spec make_derivatives(%{ required(independent_value()) => dependent_value() }) :: %{ required(independent_value()) => curvature() }
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
       (0, 0)                 -> 2 / (x1 - x0)
       (0, 1)                 -> 1 / (x1 - x0)
       # =
       (0, j) when j == n + 1 -> 3.0 * ((y1 - y0) / :math.pow(x1 - x0, 2))

       # last row
       (^n, j) when j == n - 1 -> 1 / (xn - xn_1)
       (^n, ^n)                -> 2 / (xn - xn_1)
       # =
       (^n, j) when j == n + 1 -> 3.0 * ((yn - yn_1) / :math.pow(xn - xn_1, 2))

       # middle rows
       (i, j) when j == i - 1 ->
         [xi_1, xi] = Enum.map(-1..0, fn (offset) -> Enum.at(xs, i + offset) end)
         1.0 / (xi - xi_1)
       (i, i) ->
         [xi_1, xi, xi1] = Enum.map(-1..1, fn (offset) -> Enum.at(xs, i + offset) end)
         2.0 * ((1.0 / (xi - xi_1)) + (1.0 / (xi1 - xi)))
       (i, j) when j == i + 1 ->
         [xi, xi1] = Enum.map(0..1, fn (offset) -> Enum.at(xs, i + offset) end)
         1.0 / (xi1 - xi)
       # =
       (i, j) when j == n + 1 ->
         [xi_1, xi, xi1] = Enum.map(-1..1, fn (offset) -> Enum.at(xs, i + offset) end)
         [yi_1, yi, yi1] = [xi_1, xi, xi1]
                            |> Enum.map(&(Map.get(points, &1)))
         3.0 * (
           ((yi - yi_1) / :math.pow(xi - xi_1, 2)) +
           ((yi1 - yi) / :math.pow(xi1 - xi, 2))
         )

       # empty terms
       (_i, _j) -> 0.0
    end)

    with {:ok, {matrix, vector}}  <- Matrix.disaugment(system_of_eqns),
         {:ok, l}                 <- Matrix.cholesky_decomposition(matrix),
         {:ok, y}                 <- Matrix.forward_substitution(l, vector),
         {:ok, derivative_vector} <- l |> Matrix.transpose |> Matrix.backward_substitution(y) do
      Enum.zip(xs, Vector.to_list(derivative_vector)) |> Map.new
    end
  end
end
