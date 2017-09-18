-- Note: this is a sample applist file (I use a file called "applist.lua" which exists in
-- my machine-specific dotfiles repo). Use this file as a template for a new applist.lua
-- Get default grid size
local grid = hs.grid.getGrid()

local applist = {
  { key = "s", app = "iTunes", fullscreen = true, screen = 1 },
  { key = "e", app = "Emacs", fullscreen = true, screen = 2 },
  { key = "m", app = "Mailplane 3", fullscreen = true, screen = 1 },
  { key = "t", app = "iTerm", fullscreen = true, screen = 2 },
  { key = "g", app = "Google Chrome", layout = { x = 0, y = 0, w = grid.w, h = grid.h }, screen = 1 },
}

return applist
