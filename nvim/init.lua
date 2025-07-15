local data_home = os.getenv("XDG_DATA_HOME") or (os.getenv("HOME") .. "/.local/share")
local plug_path = data_home .. "/nvim/site/autoload/plug.vim"
local plug_install_dir = data_home .. "/nvim/plugged"

-- Auto-install vim-plug if missing
if vim.fn.empty(vim.fn.glob(plug_path)) > 0 then
  print("Installing vim-plug...")
  local install_cmd = string.format(
    [[sh -c 'curl -fLo "%s" --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim']],
    plug_path
  )
  vim.fn.system(install_cmd)
  print("vim-plug installed! Please restart Neovim.")
  return
end

vim.cmd('runtime autoload/plug.vim')

local Plug = vim.fn['plug#']


-- Begin plugin installation
vim.call('plug#begin', plug_install_dir)
Plug('ellisonleao/gruvbox.nvim', { ['as'] = 'gruvbox' })
Plug('rose-pine/neovim', { ['as'] = 'rose-pine' })
Plug('mfussenegger/nvim-lint')
Plug('MeanderingProgrammer/render-markdown.nvim') 
Plug('nvim-lualine/lualine.nvim') 
Plug('nvim-tree/nvim-web-devicons') 
Plug('nvim-treesitter/nvim-treesitter') 
Plug('numToStr/Comment.nvim') 
Plug('nvim-lua/plenary.nvim')
Plug('nvim-telescope/telescope.nvim')
Plug('MunifTanjim/nui.nvim')
-- End plugin installation
vim.call('plug#end')

-- Check if plugins folder is empty
local function plugins_empty()
  local uv = vim.loop
  local stat = uv.fs_stat(plug_install_dir)
  if not stat then return true end
  local handle = uv.fs_scandir(plug_install_dir)
  if not handle then return true end
  local name = uv.fs_scandir_next(handle)
  return name == nil
end

-- Run PlugInstall automatically if plugins folder is empty
if plugins_empty() then
  vim.api.nvim_create_autocmd("VimEnter", {
    once = true,
    callback = function()
      print("Running PlugInstall...")
      vim.cmd('PlugInstall --sync | source $MYVIMRC')
    end,
  })
end
-- Load vim-plug
require("config.theme")
require("config.map")
require("config.set")


require("plugins.telescope")
require("plugins.sudo")
load_theme()
