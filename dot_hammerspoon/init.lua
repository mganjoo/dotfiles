local mash = {"cmd", "ctrl", "alt"}

-- == Literal paste (paste as if you are typing out the characters) == {{{1
hs.hotkey.bind({"cmd", "alt"}, "v", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

-- == Grid arrangement == {{{1

GRID_WIDTH = 4
GRID_HEIGHT = 4
hs.grid.setGrid(hs.geometry.size(GRID_WIDTH, GRID_HEIGHT))

local function adjustFocusedWindowGrid(fn)
  local win = hs.window.focusedWindow()
  hs.grid.adjustWindow(fn, win)
end

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
      g.x = GRID_WIDTH / 2; g.y = GRID_HEIGHT / 2
      g.w = GRID_WIDTH / 2; g.h = GRID_HEIGHT / 2
    end
  )
end)

-- Bottom left quarter
hs.hotkey.bind(mash, '9', function()
  adjustFocusedWindowGrid(
    function(g)
      g.x = 0; g.y = GRID_HEIGHT / 2
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

-- == Litra Glow == {{{1

hs.hotkey.bind(mash, "[", function() hs.execute("light", true) end)
hs.hotkey.bind(mash, "]", function() hs.execute("dark", true) end)

-- == Final == {{{1

hs.alert.show("Window manager started.", 2)

-- vim: foldmethod=marker:fen
