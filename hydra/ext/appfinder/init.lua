-- appfinder.lua v2014.07.07
-- cmsj@tenshu.net
-- Utility functions to aid finding:
--   * Applications
--   * Windows

ext.appfinder = {}

-- Internal function to search all windows using a matching function
local function find_window_from_function(fn)
    return fnutils.find(window.allwindows(), fn)
end

-- Internal function to turn a matching function and window title into an application object
local function find_application_from_window(title, fn)
    local w = find_window_from_function(fn)
    if w then
        return w:application()
    else
        return nil
    end
end

-- Finds an application by its name
--  Parameters:
--      name: Application name (e.g. "Safari")
--  Returns:
--      application object or nil
function ext.appfinder.app_from_name(name)
    return fnutils.find(application.runningapplications(), function(app) return app:title() == name end)
end

-- Finds an application by its window title
--  Parameters:
--      title: Window title (e.g. "Activity Monitor (All Processes)")
--  Returns:
--      application object or nil
function ext.appfinder.app_from_window_title(title)
    return find_application_from_window(title, function(win) return win:title() == title end)
end

-- Finds an application by pattern in its window title
--  Parameters:
--      pattern: Lua pattern (e.g. "Inbox %(%d+ messages.*")
--  Returns:
--      application object or nil
--  Notes:
--      For more about Lua patterns, see:
--       http://lua-users.org/wiki/PatternsTutorial
--       http://www.lua.org/manual/5.2/manual.html#6.4.1
function ext.appfinder.app_from_window_title_pattern(pattern)
    return find_application_from_window(pattern, function(win) return string.match(win:title(), pattern) end)
end

-- Finds a window by its title
--  Parameters:
--    title: Window title (e.g. "Activity Monitor (All Processes)")
--  Returns:
--    window object or nil
function ext.appfinder.window_from_window_title(title)
    return find_window_from_function(function(win) return win:title() == title end)
end
