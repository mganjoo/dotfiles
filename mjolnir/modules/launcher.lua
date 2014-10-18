-- Configure launcher for various applications. --

local application = require("mjolnir.application")
local grid        = require("mjolnir.bg.grid")
local appfinder   = require("mjolnir.cmsj.appfinder")
local fnutils     = require("mjolnir.fnutils")
local hotkey      = require("mjolnir.hotkey")
local alert       = require("mjolnir.alert")
local screen      = require("mjolnir.screen")
local window      = require("mjolnir.window")
local retry       = require("shared/retry")
local tableutil   = require("shared/tableutil")

-- TODO: document the applist table.

local launcher = {}

local FULL_FRAME = { x = 0, y = 0, w = grid.GRIDWIDTH, h = grid.GRIDHEIGHT }

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

local function launchandarrangeapps(applist)

  alert.show("Launching apps...", 2)
  local success = true
  local oldw = window.focusedwindow()

  -- Refresh monitor table.
  local screens = sortedscreens()
  local screencount = tableutil.tablesize(screens)

  fnutils.each(applist, function(e)
    application.launchorfocus(e.app)
    local a = appfinder.app_from_name(e.app)

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
            grid.set(win, FULL_FRAME, screens[screenidx])
            win:setfullscreen(true)
          end
          win:setfullscreen(true)
        else
          if winisfull then
            win:setfullscreen(false)
          end
          grid.set(win, e.layout, screens[screenidx])
        end
      else
        alert.show(errormessage(e.app, "could not acquire main window"), 2)
        success = false
      end
    else
      alert.show(errormessage(e.app, "could not get app with specified name"), 2)
      success = false
    end
  end)
  if oldw ~= nil then oldw:focus() end

  if success then
    alert.show("Finished launching apps.", 2)
  else
    alert.show("Some apps didn't load correctly; try again.", 2)
  end

end

function launcher.initbindings(applist, mash)
  -- Set up keybindings.
  fnutils.each(applist, function(e)
    hotkey.bind(mash, e.key, function() application.launchorfocus(e.app) end)
  end)

  -- Set up layout configuration hotkey.
  hotkey.bind(mash, "x", function() launchandarrangeapps(applist) end)
end

return launcher
