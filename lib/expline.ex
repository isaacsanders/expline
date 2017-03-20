defmodule Expline do
  use GenServer
  require Logger

  @moduledoc """
  `Expline` is a `GenServer` that wraps the `Expline.Spline` module. It builds
  the `Expline.Spline` asynchronously in a `Task` after being supplied the input
  parameters in `initialize/2`. After initializing, use the `interpolate/2` and
  `interpolate/3` functions to find the corresponding points for each value you
  wish to interpolate. If the server is not initialized, it will return an
  error.

  For more information regarding the mathematics and performance of the spline
  building, read the `Expline.Spline` module documentation.
  """

  @typep initializing_state() :: {:initializing, Task.t}
  @typep state() :: :uninitialized
                  | initializing_state()
                  | {:ready, Expline.Spline.t}

  @typedoc """
  The point type used to initialize `Expline` and returned from `interpolate/2`
  and `interpolate/3`.
  """
  @type point() :: {float(), float()}

  def init(_) do
    {:ok, :uninitialized}
  end

  @doc """
  Asynchronously starts a `Task` that builds the internal `Expline.Spline` from
  a list of `t:point/0`.

  This call will log warnings if the spline is being built or if the server is
  ready.

  In either case, it will proceed with the instruction and build a new spline
  from the given parameters. In future releases, there may be options to
  silence the logging or to ignore the initialization call if the proper
  option is set, depending on the feedback from users.
  """

  @spec initialize(GenServer.server(), list(point())) :: :ok
  def initialize(server, points) when is_list(points) do
    GenServer.cast(server, {:initialize, points})
  end

  @spec handle_cast({:initialize, list(point())}, state()) ::
    {:noreply, initializing_state()}
  def handle_cast({:initialize, points}, old_state) do
    case old_state do
      :uninitialized ->
        nil
      {:initializing, task} ->
        Logger.warn("Shutting down Expline initialization task to reinitialize with new initial parameters")
        Task.shutdown(task, :brutal_kill)
      {:ready, _spline} ->
        Logger.warn("Initializing an Expline server that has already been initialized")
    end

    new_state = {:initializing, Task.async(fn ->
      {:ready_spline, Expline.Spline.from_points(points)}
    end)}

    {:noreply, new_state}
  end

  # When the initializing task finishes, it sends this message to the server.
  def handle_info({_task_ref, {:ready_spline, spline}}, _state) do
    {:noreply, {:ready, spline}}
  end

  # When the initializing task finishes, it sends this message to the server as
  # it dies and cleans up.
  def handle_info({:DOWN, _task_ref, :process, _pid, :normal}, state) do
    {:noreply, state}
  end

  @doc """
  Interpolate a `t:point/0` from its independent value.

  When the server is uninitialized or building the spline, the return value
  will be `{:error, :server_not_ready}`.
  """

  @spec interpolate(GenServer.server(), float(), timeout()) ::
    {:ok, point()} |
    {:error, :server_not_ready}
  def interpolate(server, x, timeout \\ 5000) when is_float(x) do
    GenServer.call(server, {:interpolate, x}, timeout)
  end

  @spec handle_call({:interpolate, float()}, GenServer.from(), state()) ::
    {:reply, {:ok, point()}, state()} |
    {:reply, {:error, :server_not_ready}, state()}
  def handle_call({:interpolate, x}, _from, state) do
    case state do
      {:ready, spline} ->
        {:ok, y} = Expline.Spline.interpolate(spline, x)
        {:reply, {:ok, {x, y}}, state}
      _ ->
        {:reply, {:error, :server_not_ready}, state}
    end
  end
end
