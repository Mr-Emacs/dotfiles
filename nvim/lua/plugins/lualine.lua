local colors = {
  bg        = '#161616',
  fg        = '#ffffff',
  hl_line   = '#101040',
  selection = '#3a3a8f',
  bracket   = '#cdaa7d',
  comment   = '#7f7f7f',
  keyword   = '#cd950c',
  command   = '#61afef',
  purple    = '#654CA8',
}

local custom_dark = {
  normal = {
    a = { fg = colors.bg, bg = colors.bracket, gui = 'bold' },
    b = { fg = colors.fg, bg = colors.hl_line },
    c = { fg = colors.fg, bg = colors.bg },
  },
  insert = {
    a = { fg = colors.bg, bg = colors.command, gui = 'bold' },
    b = { fg = colors.fg, bg = colors.hl_line },
  },
  visual = {
    a = { fg = colors.bg, bg = colors.purple, gui = 'bold' },
    b = { fg = colors.fg, bg = colors.hl_line },
  },
  replace = {
    a = { fg = colors.bg, bg = colors.keyword, gui = 'bold' },
    b = { fg = colors.fg, bg = colors.hl_line },
  },
  command = {
    a = { fg = colors.bg, bg = colors.command, gui = 'bold' },
    b = { fg = colors.fg, bg = colors.hl_line },
  },
  inactive = {
    a = { fg = colors.comment, bg = colors.bg },
    b = { fg = colors.comment, bg = colors.bg },
    c = { fg = colors.comment, bg = colors.bg },
  },
}

require("lualine").setup({
    options = {
        icons_enabled = false,
        theme = custom_dark,
        component_separators = "",
        section_separators = "",
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch" },
        lualine_c = { "filename" },
        lualine_x = {
            function()
                local encoding = vim.o.fileencoding
                if encoding == "" then
                    return vim.bo.fileformat .. " :: " .. vim.bo.filetype
                else
                    return encoding .. " :: " .. vim.bo.fileformat .. " :: " .. vim.bo.filetype
                end
            end,
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
    },
})
