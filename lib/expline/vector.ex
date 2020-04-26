defmodule Expline.Vector do
  @moduledoc false

  @enforce_keys [:n_slots, :internal]
  defstruct [:n_slots, :internal]

  @type t() :: %Expline.Vector{n_slots: pos_integer(), internal: internal()}
  @typep internal() :: tuple()

  @spec construct(pos_integer(), (non_neg_integer() -> float())) :: Expline.Vector.t()
  def construct(n_slots, elem_fn)
      when n_slots > 0 and
             is_function(elem_fn, 1) do
    internal =
      Enum.reduce(0..(n_slots - 1), {}, fn i, vec ->
        Tuple.append(vec, elem_fn.(i))
      end)

    %Expline.Vector{n_slots: n_slots, internal: internal}
  end

  @spec at(Expline.Vector.t(), non_neg_integer()) :: float()
  def at(%Expline.Vector{n_slots: n_slots, internal: internal}, i)
      when is_integer(i) and
             i < n_slots do
    elem(internal, i)
  end

  def to_list(%Expline.Vector{} = vector) do
    Tuple.to_list(vector.internal)
  end
end
