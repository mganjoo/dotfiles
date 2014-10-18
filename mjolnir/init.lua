-- Home directory.
local home = os.getenv("HOME")

-- Load packages.
local alert       = require("mjolnir.alert")
local pathtools   = require("shared/pathtools")
local pathwatcher = require("mjolnir._asm.pathwatcher")

-- Watch paths for changes.
local watchpaths = {
  "/.mjolnir/",
  "/.mjolnir.local"
}
for _, p in ipairs(watchpaths) do
  pathwatcher.new(home .. p, mjolnir.reload):start()
end

-- Load machine-specific configuration file (with constants etc.).
pathtools.addtopath(home .. "/.mjolnir.local/?.lua", true)
configfile = package.searchpath("config", package.path)
if configfile ~= nil then dofile(configfile) end

-- Configure mash keys.
mash = {"cmd", "ctrl", "alt"}

-- Configure which modules are to be loaded.
local modules = {
  "arrange",
  "launcher"
}
for _, m in ipairs(modules) do
  local name = "modules/" .. m
  local mod = require(name)
  mod.init()
end

-- Finish loading.
alert.show("Mjolnir started.", 2)
