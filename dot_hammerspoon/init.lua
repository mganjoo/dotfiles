local mash = {"cmd", "ctrl", "alt"}
hs.logger.defaultLogLevel = "info"
hs.loadSpoon("SpoonInstall")

-- == Literal paste (paste as if you are typing out the characters) == {{{1
hs.hotkey.bind({"cmd", "alt"}, "v", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

-- == Grid arrangement == {{{1

spoon.SpoonInstall:andUse("WindowHalfsAndThirds",
  {
    config = {
      use_frame_correctness = false
    },
    hotkeys = {
      max = {mash, "1"},
      left_half = {mash, "2"},
      top_half = {mash, "3"},
      right_half = {mash, "4"},
      bottom_half = {mash, "5"},
      top_left = {mash, "6"},
      top_right = {mash, "7"},
      bottom_right = {mash, "8"},
      bottom_left = {mash, "9"},
      center = {mash, "-"},
    }
  }
)

-- Push to next screen
hs.hotkey.bind(mash, '0', function()
  local win = hs.window.focusedWindow()
  win:moveToScreen(win:screen():next())
end)

-- }}}2

-- == iTunes == {{{1

hs.hotkey.bind(mash, "i", hs.itunes.displayCurrentTrack)

-- == Litra Glow == {{{1

hs.hotkey.bind(mash, "[", function() hs.execute("light", true) end)
hs.hotkey.bind(mash, "]", function() hs.execute("dark", true) end)

-- == VimMode == {{{1

local VimMode = hs.loadSpoon("VimMode")
local vim = VimMode:new()

-- Apps for which we don't want VimMode
vim
  :disableForApp("Code")
  :disableForApp("zoom.us")
  :disableForApp("Ghostty")
  :disableForApp("Terminal")

-- Fallback mode (bind to built-in text shortcuts)
-- Only Chrome and Safari (not Firefox)
vim:setFallbackOnlyUrlPatterns({
  "docs.google.com",
})

-- Dim when we enter normal mode?
vim:shouldDimScreenInNormalMode(false)

-- Show on-screen alert when we enter normal mode?
vim:shouldShowAlertInNormalMode(true)

-- You can configure your on-screen alert font
vim:setAlertFont("Courier New")

-- Enter normal mode by typing a key sequence
vim:bindHotKeys({ enter = {mash, ";"} })

-- == Final == {{{1

hs.alert.show("Window manager started.", 2)

-- vim: foldmethod=marker:fen
