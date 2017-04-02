defmodule Expline.MatrixTest do
  use ExUnit.Case
  use Quixir

  describe "Cholesky decomposition" do
    test "https://en.wikipedia.org/wiki/Cholesky_decomposition#Statement" do
      ptest n_rows: int(min: 2, max: 100) do
        a = Expline.Matrix.construct(n_rows, n_rows, fn
          (_i, _j) ->
            :rand.uniform
        end)

        matrix = Expline.Matrix.add(a, Expline.Matrix.transpose(a))
                |> Expline.Matrix.scale(0.5)
                |> Expline.Matrix.add(
                  n_rows
                  |> Expline.Matrix.identity()
                  |> Expline.Matrix.scale(1.0 * n_rows)
                )

        case Expline.Matrix.cholesky_decomposition(matrix) do
          {:ok, l} ->
            {:ok, product} = Expline.Matrix.product(l, Expline.Matrix.transpose(l))
            diff = Expline.Matrix.sub(matrix, product)
                    |> Expline.Matrix.transform(&(Float.round(&1, 13)))

            assert Expline.Matrix.zeros(n_rows, n_rows) == diff
          _error ->
            assert false
        end
      end
    end
  end
end
