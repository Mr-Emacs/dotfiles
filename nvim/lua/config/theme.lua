local theme_file = vim.fn.stdpath("config") .. "/lua/config/saved_theme"

_G.load_theme = function()
	local file = io.open(theme_file, "r")
	if file then
		local colorscheme = file:read("*l")
		local lualine_theme = file:read("*l")
		vim.cmd("colorscheme " .. colorscheme)
		require("lualine").setup({ options = { theme = lualine_theme } })

		-- Set transparent background after theme is applied
		vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
		file:close()
	end
end

local themes = {
	{ "rose-pine", "rose-pine" },
	{ "gruvbox", "gruvbox" },
}

local current_theme_index = 1

_G.switch_theme = function()
	current_theme_index = current_theme_index % #themes + 1
	local colorscheme, lualine_theme = unpack(themes[current_theme_index])
	vim.cmd("colorscheme " .. colorscheme)
	require("lualine").setup({ options = { theme = lualine_theme } })

	-- Set transparent background again after theme switch
	vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

	local file = io.open(theme_file, "w")
	if file then
		file:write(colorscheme .. "\n" .. lualine_theme)
		file:close()
	end
end

