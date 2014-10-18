-- Home directory.
local home = os.getenv("HOME")

-- Load packages.
local alert       = require("mjolnir.alert")
local pathtools   = require("shared/pathtools")
local arrange     = require("modules/arrange")
local launcher    = require("modules/launcher")

-- Load machine-specific configuration file (with constants etc.).
pathtools.addtopath(home .. "/.mjolnir.local/?.lua", true)
configfile = package.searchpath("config", package.path)
if configfile ~= nil then
  config = dofile(configfile)
end

-- Initialize key bindings.
local mash = {"cmd", "ctrl", "alt"}
arrange.initbindings(mash)
if config ~= nil then
  launcher.initbindings(config.applist, mash)
end

-- Finish loading.
alert.show("Window manager started.", 2)
