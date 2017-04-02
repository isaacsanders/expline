defmodule Expline.Mixfile do
  use Mix.Project

  def project do
    [app: :expline,
     version: "0.1.0",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),

     # Hex parameters
     description: description(),
     package: package(),

     # Type-checking flags
     dialyzer: dialyzer(),

     # Generated documentation parameters
     name: "Expline",
     source_url: "https://github.com/isaacsanders/expline",
     docs: docs()]
  end

  def application do
    [extra_applications: []]
  end

  def description do
    """
    A cubic spline interpolation library for Elixir.

    Includes a GenServer. All required Linear Algebra modules are built for the
    library, but may be extracted individually.
    """
  end

  def package do
    [name: :expline,
     files: ["lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
     licenses: ["MIT"],
     maintainers: ["Isaac Sanders"],
     links: %{"GitHub" => "https://github.com/isaacsanders/expline",
              "Documentation" => "http://hexdocs.pm/expline/"}]
  end

  def dialyzer do
    [flags: [:error_handling, :no_behaviours, :no_contracts, :no_fail_call,
      :no_fun_app, :no_improper_lists, :no_match, :no_missing_calls, :no_opaque,
      :no_return, :no_undefined_callbacks, :no_unused, :race_conditions,
      :underspecs, :unknown]]
  end

  def docs do
    [main: "Expline",
     extras: ["README.md"]]
  end

  defp deps do
    [{:ex_doc,   "~> 0.14", only: :dev, runtime: false},
     {:dialyxir, "~> 0.5",  only: :dev, runtime: false},
     {:quixir,   "~> 0.9",  only: :test }]
  end
end
