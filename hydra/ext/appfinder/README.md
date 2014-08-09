### Install

~~~bash
$ git clone https://github.com/cmsj/hydra-appfinder.git ~/.hydra/ext/appfinder
~~~

### Require

~~~lua
require "ext.appfinder.init"
~~~

### Core Functions

~~~lua
function ext.appfinder.app_from_name(name)  -- Returns the Hydra application object for an application with the supplied name (e.g. "Safari")
function ext.appfinder.app_from_window_title(title) -- Returns the Hydra application object for a window with the supplied title (e.g. "Activity Monitor (All Processes)")
function ext.appfinder.app_from_window_title_pattern(pattern) -- Returns the Hydra application object for a window title matching the supplied Lua pattern (e.g. "Inbox %(%d+ messages.\*")
function ext.appfinder.window_from_window_title(title) -- Returns the Hydra window object for a window with the supplied title (e.g. "Activity Monitor (All Processes)")
~~~

### Additional documentation

For more information about Lua patterns, see:

- http://lua-users.org/wiki/PatternsTutorial
- http://www.lua.org/manual/5.2/manual.html#6.4.1

