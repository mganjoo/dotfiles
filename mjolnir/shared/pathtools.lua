require 'package'

local pathtools = {}

function pathtools.addtopath(component, prepend)
  for i in package.path:gmatch("[^;]+") do
    if i == component then return false end
  end
  if prepend then
    package.path = component .. ";" .. package.path
  else
    package.path = package.path .. ";" .. component
  end
end

return pathtools
