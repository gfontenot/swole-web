use Mix.Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :swole, SwoleWeb.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Configure your database
config :swole, Swole.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "swole_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox
