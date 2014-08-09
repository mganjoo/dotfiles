local tableutil = {}

function tableutil.tablesize(tab)
  local count = 0
  for _ in pairs(tab) do
    count = count + 1
  end
  return count
end

return tableutil
