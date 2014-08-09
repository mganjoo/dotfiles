-- Bindings for various grid locations.

require "ext.grid.init"

ext.grid.GRIDWIDTH = 4
ext.grid.GRIDHEIGHT = 4

local grid = {}

local function lefthalf()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT
    end
  )
end

local function tophalf()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = ext.grid.GRIDWIDTH; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

local function righthalf()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = ext.grid.GRIDWIDTH / 2; g.y = 0
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT
    end
  )
end

local function bottomhalf()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = ext.grid.GRIDHEIGHT / 2
      g.w = ext.grid.GRIDWIDTH; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

local function center()
  local borderw = ext.grid.GRIDWIDTH / 8
  local borderh = ext.grid.GRIDHEIGHT / 8
  ext.grid.adjust_focused_window(
    function(g)
      g.x = borderw; g.y = borderh
      g.w = ext.grid.GRIDWIDTH - borderw * 2
      g.h = ext.grid.GRIDHEIGHT - borderh * 2
    end
  )
end

local function topleftquarter()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = 0
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

local function toprightquarter()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = ext.grid.GRIDWIDTH / 2; g.y = 0
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

local function bottomleftquarter()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = 0; g.y = ext.grid.GRIDWIDTH / 2
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

local function bottomrightquarter()
  ext.grid.adjust_focused_window(
    function(g)
      g.x = ext.grid.GRIDWIDTH / 2; g.y = ext.grid.GRIDHEIGHT / 2
      g.w = ext.grid.GRIDWIDTH / 2; g.h = ext.grid.GRIDHEIGHT / 2
    end
  )
end

function grid.init()
  hotkey.bind(mash, '1', ext.grid.maximize_window)
  hotkey.bind(mash, '2', lefthalf)
  hotkey.bind(mash, '3', tophalf)
  hotkey.bind(mash, '4', righthalf)
  hotkey.bind(mash, '5', bottomhalf)
  hotkey.bind(mash, '6', topleftquarter)
  hotkey.bind(mash, '7', toprightquarter)
  hotkey.bind(mash, '8', bottomrightquarter)
  hotkey.bind(mash, '9', bottomleftquarter)
  hotkey.bind(mash, '0', ext.grid.pushwindow_nextscreen)
  hotkey.bind(mash, '-', center)
end

return grid
