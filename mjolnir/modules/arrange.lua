-- Bindings for various grid arrangements.

local grid   = require("mjolnir.bg.grid")
local hotkey = require("mjolnir.hotkey")

grid.GRIDWIDTH = 4
grid.GRIDHEIGHT = 4

local arrange = {}

local function lefthalf()
  grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT
    end
  )
end

local function tophalf()
  grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = grid.GRIDWIDTH; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

local function righthalf()
  grid.adjust_focused_window(
    function(g)
      g.x = grid.GRIDWIDTH / 2; g.y = 0
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT
    end
  )
end

local function bottomhalf()
  grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = grid.GRIDHEIGHT / 2
      g.w = grid.GRIDWIDTH; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

local function center()
  local borderw = grid.GRIDWIDTH / 8
  local borderh = grid.GRIDHEIGHT / 8
  grid.adjust_focused_window(
    function(g)
      g.x = borderw; g.y = borderh
      g.w = grid.GRIDWIDTH - borderw * 2
      g.h = grid.GRIDHEIGHT - borderh * 2
    end
  )
end

local function topleftquarter()
  grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

local function toprightquarter()
  grid.adjust_focused_window(
    function(g)
      g.x = grid.GRIDWIDTH / 2; g.y = 0
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

local function bottomleftquarter()
  grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = grid.GRIDWIDTH / 2
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

local function bottomrightquarter()
  grid.adjust_focused_window(
    function(g)
      g.x = grid.GRIDWIDTH / 2; g.y = grid.GRIDHEIGHT / 2
      g.w = grid.GRIDWIDTH / 2; g.h = grid.GRIDHEIGHT / 2
    end
  )
end

function arrange.init()
  hotkey.bind(mash, '1', grid.maximize_window)
  hotkey.bind(mash, '2', lefthalf)
  hotkey.bind(mash, '3', tophalf)
  hotkey.bind(mash, '4', righthalf)
  hotkey.bind(mash, '5', bottomhalf)
  hotkey.bind(mash, '6', topleftquarter)
  hotkey.bind(mash, '7', toprightquarter)
  hotkey.bind(mash, '8', bottomrightquarter)
  hotkey.bind(mash, '9', bottomleftquarter)
  hotkey.bind(mash, '0', grid.pushwindow_nextscreen)
  hotkey.bind(mash, '-', center)
end

return arrange
