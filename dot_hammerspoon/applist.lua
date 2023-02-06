-- Get default grid size
local grid = hs.grid.getGrid()

local applist = {
  { key = "s", app = "iTunes", fullscreen = true, screen = 1 },
  { key = "g", app = "Google Chrome", layout = { x = 0, y = 0, w = grid.w, h = grid.h }, screen = 1 },
}

return applist
