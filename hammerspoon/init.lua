-- == Support functions == {{{1

local function errorMessage(appName, message)
  return string.format("%s: %s", appName, message)
end

local function adjustFocusedWindowGrid(fn)
  local win = hs.window.focusedWindow()
  hs.grid.adjustWindow(fn, win)
end

local function sortedScreens()
  -- Sort screens in ascending order of size.
  local screens = hs.screen.allScreens()
  table.sort(screens, function(a, b)
    return a:fullFrame().h < b:fullFrame().h
  end)
  return screens
end

local function retryWithTimeout(fn, maxAttempts, sleepTime)
  local x = fn()
  local attempts = 0
  while x == nil and attempts < maxAttempts do
    os.execute("sleep " .. tonumber(sleepTime))
    x = fn()
    attempts = attempts + 1
  end
  return x
end

local function launchAndArrangeApps(applist, moveFullScreens)

  hs.alert.show("Launching apps...", 2)

  local success = true
  local oldw = hs.window.focusedWindow()

  -- Refresh monitor table.
  local screens = sortedScreens()

  hs.fnutils.each(applist, function(e)
    hs.application.launchOrFocus(e.app)
    local a = hs.application(e.app)

    if not e.fullscreen and e.layout == nil then
      error(errorMessage(e.app, "no layout specification provided"))
    end

    local screenidx = math.min(#screens, e.screen)

    if a then
      local win = retryWithTimeout(function()
        return a:mainWindow()
      end, 5, 0.1)
      if not win then win = a:focusedWindow() end
      if win then
        local isFullScreen = win:isFullScreen()
        if e.fullscreen then
          -- Change to not fullscreen first so they can be moved around
          if moveFullScreens and isFullScreen then
            win:setFullScreen(false)
          end
          if moveFullScreens or not isFullScreen then
            hs.grid.set(win, { x = 0, y = 0, w = GRID_WIDTH, h = GRID_HEIGHT }, screens[screenidx])
          end
          win:setFullScreen(true)
        else
          win:setFullScreen(false)
          hs.grid.set(win, e.layout, screens[screenidx])
        end
      else
        hs.alert.show(errorMessage(e.app, "could not acquire main window"), 2)
        success = false
      end
    else
      hs.alert.show(errorMessage(e.app, "could not get app with specified name"), 2)
      success = false
    end
  end)

  if oldw then oldw:focus() end

  if success then
    hs.alert.show("Finished launching apps.", 2)
  else
    hs.alert.show("Some apps didn't load correctly; try again.", 2)
  end

end

-- == Basic settings == {{{1

local mash = {"cmd", "ctrl", "alt"}

-- == Useful hotkeys == {{{1
hs.hotkey.bind({"cmd", "alt"}, "v", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

-- == Grid == {{{1

-- Set grid dimensions
GRID_WIDTH = 4
GRID_HEIGHT = 4
hs.grid.setGrid(hs.geometry.size(GRID_WIDTH, GRID_HEIGHT))

-- Set bindings {{{2

-- Maximize window
hs.hotkey.bind(mash, '1', hs.grid.maximizeWindow)

-- Left half
hs.hotkey.bind(mash, '2', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = 0
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT
    end
  )
end)

-- Top half
hs.hotkey.bind(mash, '3', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = 0
      g.w = GRID_WIDTH; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Right half
hs.hotkey.bind(mash, '4', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = GRID_WIDTH / 2; g.y = 0
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT
    end
  )
end)

-- Bottom half
hs.hotkey.bind(mash, '5', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = GRID_HEIGHT / 2
      g.w = GRID_WIDTH; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Top left quarter
hs.hotkey.bind(mash, '6', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = 0
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Top right quarter
hs.hotkey.bind(mash, '7', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = GRID_WIDTH / 2; g.y = 0
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Bottom right quarter
hs.hotkey.bind(mash, '8', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = GRID_WIDTH / 2
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Bottom left quarter
hs.hotkey.bind(mash, '9', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = GRID_WIDTH / 2; g.y = GRID_HEIGHT / 2
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Push to next screen
hs.hotkey.bind(mash, '0', function()
  local win = hs.window.focusedWindow()
  local grid = hs.grid.get(win)
  hs.grid.set(win, grid, win:screen():next())
end)

-- Center
hs.hotkey.bind(mash, '-', function()
  local borderW = GRID_WIDTH / 8
  local borderH = GRID_HEIGHT / 8
  adjustFocusedWindowGrid(
    function(g)
      g.x = borderW; g.y = borderH
      g.w = GRID_WIDTH - borderW * 2
      g.h = GRID_HEIGHT - borderH * 2
    end
  )
end)

-- }}}2

-- == iTunes == {{{1

hs.hotkey.bind(mash, "i", hs.itunes.displayCurrentTrack)

-- == GlobalProtect == {{{1
hs.hotkey.bind(mash, "v", function()
  hs.applescript([[
  tell application "System Events" to tell process "GlobalProtect"
    click menu bar item 1 of menu bar 2
    click button 2 of window 1 -- Either Connect or Disconnect
    click menu bar item 1 of menu bar 2 -- Close the window (optional).
  end tell
  ]])
end)

-- == Machine-specific application list == {{{1

local applist
-- Note: see "applist_sample.lua" for an example of an applist
local applistFn = loadfile("applist.lua")
if applistFn then
  applist = applistFn()
end

if applist then
  hs.fnutils.each(applist, function(e)
    hs.hotkey.bind(mash, e.key, function() hs.application.launchOrFocus(e.app) end)
  end)

  -- Set up layout configuration hotkey.
  hs.hotkey.bind(mash, "x", function() launchAndArrangeApps(applist, false) end)
  hs.hotkey.bind(mash, "z", function() launchAndArrangeApps(applist, true) end)
end

-- == Final == {{{1

hs.alert.show("Window manager started.", 2)

-- vim: foldmethod=marker:fen
