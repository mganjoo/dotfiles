-- Load reloadable files
import = require("util/import")
import.clear_cache()
pathtools = import("util/pathtools")

-- Configure additional load path.
pathtools.addtopath(os.getenv("HOME") .. "/.hydra.local/?.lua", true)

-- Configure reloading.
hydra.autolaunch.set(true)
pathwatcher.new(os.getenv("HOME") .. "/.hydra/", hydra.reload):start()
pathwatcher.new(os.getenv("HOME") .. "/.hydra.local/", hydra.reload):start()

-- Load machine-specific configuration file (with constants etc.).
configfile = package.searchpath("config", package.path)
if configfile ~= nil then dofile(configfile) end

-- Configure mash keys.
mash = {"cmd", "ctrl", "alt"}

-- Configure which modules are to be loaded.
local modules = {
  "grid",
  "launcher"
}
for _, m in ipairs(modules) do
  local name = "modules/" .. m
  local mod = import(name)
  mod.init()
end

-- Configure Hydra menu.
hydra.menu.show(function()
    return {
      {title = "Reload Config", fn = hydra.reload},
      {title = "-"},
      {title = "About Hydra", fn = hydra.showabout},
      {title = "Check for Updates...", fn = function() hydra.updates.check(nil, true) end},
      {title = "Quit", fn = os.exit},
    }
end)

-- Configure updates.
notify.register("showupdate", function() os.execute('open https://github.com/sdegutis/Hydra/releases') end)
timer.new(timer.weeks(1), hydra.updates.check):start()
hydra.updates.check()

hydra.alert "Hydra started."
