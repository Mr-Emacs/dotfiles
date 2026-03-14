local M = {}

local telescope = require("telescope")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")

telescope.setup({})

function M.word_grep()
  local word = vim.fn.expand("<cword>")
  builtin.live_grep({
    default_text = word,
    attach_mappings = function(_, map)
      map("i", "<C-q>", actions.send_to_qflist + actions.open_qflist)
      map("n", "<C-q>", actions.send_to_qflist + actions.open_qflist)
      return true
    end,
  })
end

vim.keymap.set("n", "<leader>fw", M.word_grep, { desc = "Grep word under cursor" })

return M
