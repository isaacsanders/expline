defmodule Expline do
  use GenServer
  require Logger

  @moduledoc """
  `Expline` is a `GenServer` that wraps the `Expline.Spline` module. It builds
  the `Expline.Spline` after being supplied the input parameters in `start/3` or
  `start_link/3`. After initializing, use the `interpolate/2` and
  `interpolate/3` functions to find the corresponding points for each value you
  wish to interpolate.

  For more information regarding the mathematics and performance of the spline
  building, read the `Expline.Spline` module documentation.
  """

  @typep state() :: Expline.Spline.t

  @doc """
  Builds a spline from the provided list of points and holds the state in a
  process without links (outside of a supervision tree).

  See `start_link/2` for more information.
  """
  @spec start(list(Expline.Spline.point()), GenServer.options()) :: {:ok, pid()}
                                                                  | {:error, {:already_started, pid()}}
                                                                  | {:error, Expline.Spline.creation_error()}
  def start(points, opts \\ []) do
    GenServer.start(__MODULE__, [points], opts)
  end

  @doc """
  Builds a spline from the provided list of points and holds the state in a
  process linked to the current process.

  This is often used to start the server process as part of a supervision tree.

  ## Options and more information

  See `GenServer.start_link/3` for more information.
  """
  @spec start_link(list(Expline.Spline.point()), GenServer.options()) :: {:ok, pid()}
                                                                       | {:error, {:already_started, pid()}}
                                                                       | {:error, Expline.Spline.creation_error()}
  def start_link(points, opts \\ []) do
    GenServer.start_link(__MODULE__, [points], opts)
  end

  def init([list_of_points]) do
    case Expline.Spline.from_points(list_of_points) do
      {:ok, spline} ->
        {:ok, spline}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  @doc """
  Interpolate a `t:Expline.Spline.point/0` from its independent value.

  When an error arises with the interpolation, an error found in
  `t:Expline.Spline.interpolation_error/0` will be returned.
  """

  @spec interpolate(GenServer.server(), float(), timeout()) :: {:ok, Expline.Spline.point()}
                                                             | {:error, Expline.Spline.interpolation_error()}
  def interpolate(server, x, timeout \\ 5000) when is_float(x) do
    GenServer.call(server, {:interpolate, x}, timeout)
  end

  @spec handle_call({:interpolate, Expline.Spline.dependent_value()}, GenServer.from(), state()) :: {:reply, {:ok, Expline.Spline.point()}, state()}
  def handle_call({:interpolate, x}, _from, spline) do
    case Expline.Spline.interpolate(spline, x) do
      {:ok, y} ->
        {:reply, {:ok, {x, y}}, spline}
      {:error, reason} ->
        {:reply, {:error, reason}, spline}
    end
  end
end
