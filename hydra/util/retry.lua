local retry = {}

function retry.getretrywithtimeout(fn, maxattempts, sleeptime)
  local x = fn()
  local attempts = 0
  while x == nil and attempts < maxattempts do
    os.execute("sleep " .. tonumber(sleeptime))
    x = fn()
    attempts = attempts + 1
  end
  return x
end

return retry
