-- Configure launcher for various applications. --

require "ext.grid.init"
require "ext.appfinder.init"
retry = import("util/retry")
tableutil = import("util/tableutil")

-- TODO: document the applist table.

local launcher = {}

local FULL_FRAME = { x = 0, y = 0, w = ext.grid.GRIDWIDTH, h = ext.grid.GRIDHEIGHT }

local function errormessage(appname, msg)
  return string.format("%s: %s", appname, msg)
end

local function sortedscreens()
  -- Sort screens by left x.
  local screens = screen.allscreens()
  table.sort(screens, function(a, b)
    return a:frame_including_dock_and_menu().x < b:frame_including_dock_and_menu().x
  end)
  return screens
end

local function launchandarrangeapps()

  hydra.alert "Launching apps..."
  local success = true
  local oldw = window.focusedwindow()

  -- Refresh monitor table.
  local screens = sortedscreens()
  local screencount = tableutil.tablesize(screens)

  fnutils.each(applist, function(e)
    application.launchorfocus(e.app)
    local a = ext.appfinder.app_from_name(e.app)

    if not e.fullscreen and e.layout == nil then
      error(errormessage(e.app, "no layout specification provided"))
    end

    local screenidx = math.min(screencount, e.screen)

    if a ~= nil then
      local win = retry.getretrywithtimeout(function() return a:mainwindow() end, 5, 0.1)
      if win ~= nil then
        local winisfull = win:isfullscreen()
        if e.fullscreen then
          if not winisfull then
            ext.grid.set(win, FULL_FRAME, screens[screenidx])
            win:setfullscreen(true)
          end
          win:setfullscreen(true)
        else
          if winisfull then
            win:setfullscreen(false)
          end
          ext.grid.set(win, e.layout, screens[screenidx])
        end
      else
        hydra.alert(errormessage(e.app, "could not acquire main window"))
        success = false
      end
    else
      hydra.alert(errormessage(e.app, "could not get app with specified name"))
      success = false
    end
  end)
  if oldw ~= nil then oldw:focus() end

  if success then
    hydra.alert "Finished launching apps."
  else
    hydra.alert "Some apps didn't load correctly; try again."
  end

end

function launcher.init()
  if applist ~= nil then
    -- Set up keybindings.
    fnutils.each(applist, function(e)
      hotkey.bind(mash, e.key, function() application.launchorfocus(e.app) end)
    end)

    -- Set up layout configuration hotkey.
    hotkey.bind(mash, "x", launchandarrangeapps)
  end
end

return launcher
