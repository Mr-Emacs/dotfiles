local uv = vim.loop

local M = {}

local function create_askpass_script(password)
  local fd, path = uv.fs_mkstemp("/tmp/nvim-askpass-XXXXXX")
  if not fd then
    vim.notify("Failed to create askpass script: " .. tostring(path), vim.log.levels.ERROR)
    return nil
  end
  -- Properly escape the password for the shell script
  local escaped_password = string.gsub(password, "'", "'\"'\"'")
  local content = "#!/bin/sh\nprintf '%s' '" .. escaped_password .. "'"
  uv.fs_write(fd, content, -1)
  uv.fs_close(fd)
  os.execute("chmod +x " .. path)
  return path
end

local function sudo_write(filepath, password)
  -- Create a temporary file with current buffer content
  local tmpfile = os.tmpname()
  vim.cmd('write! ' .. tmpfile)
  
  local askpass_path = create_askpass_script(password)
  if not askpass_path then
    vim.notify("Could not create askpass script", vim.log.levels.ERROR)
    return
  end
  
  -- Use a different approach: pipe the temp file to sudo tee
  local cmd = string.format("SUDO_ASKPASS=%s sudo -A tee %s < %s > /dev/null", 
    vim.fn.shellescape(askpass_path), 
    vim.fn.shellescape(filepath), 
    vim.fn.shellescape(tmpfile))
  
  local output = {}
  local handle = vim.fn.jobstart({"sh", "-c", cmd}, {
    on_stdout = function(_, data)
      if data then
        vim.list_extend(output, data)
      end
    end,
    on_stderr = function(_, data)
      if data then
        vim.list_extend(output, data)
      end
    end,
    on_exit = function(_, code)
      os.remove(tmpfile)
      os.remove(askpass_path)
      if code == 0 then
        vim.schedule(function()
          vim.notify("Sudo write succeeded!", vim.log.levels.INFO)
        end)
      else
        vim.schedule(function()
          local error_msg = table.concat(output, "\n")
          vim.notify("Sudo write failed (code: " .. code .. "): " .. error_msg, vim.log.levels.ERROR)
        end)
      end
    end,
  })
  
  if handle <= 0 then
    vim.notify("Failed to start sudo write job", vim.log.levels.ERROR)
    os.remove(tmpfile)
    os.remove(askpass_path)
  end
end

function M.sudo_write()
  local filepath = vim.fn.expand("%:p")
  if filepath == "" then
    vim.notify("No file to write", vim.log.levels.ERROR)
    return
  end
  
  local password = vim.fn.inputsecret("Enter sudo password: ")
  if password == "" then
    vim.notify("No password entered", vim.log.levels.WARN)
    return
  end
  
  sudo_write(filepath, password)
end

vim.api.nvim_create_user_command("SudoWrite", function()
  M.sudo_write()
end, {})

return M
