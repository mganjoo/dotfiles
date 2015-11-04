-- == Support functions == {{{1

local function adjustFocusedWindowGrid(fn)
  local win = hs.window.focusedWindow()
  hs.grid.adjustWindow(fn, win)
end

-- == Basic settings == {{{1

local mash = {"cmd", "ctrl", "alt"}

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

-- == Machine-specific application list == {{{1

local applist
local applistFn = loadfile("applist.lua")
if applistFn then
  applist = applistFn()
end

if applist then
  -- TODO: arrange windows
end

-- == Final == {{{1

hs.alert.show("Window manager started.", 2)

-- vim: foldmethod=marker:fen
