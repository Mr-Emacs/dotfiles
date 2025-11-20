local function load_plugin_configs()
    local plugin_dir = vim.fn.stdpath('config') .. '/lua/plugins'
    local files = vim.fn.readdir(plugin_dir)

    for _, file in ipairs(files) do
        local _ = plugin_dir .. '/' .. file
        if file:match("%.lua$") then
            local plugin_name = file:match("(.+).lua")
            require('plugins.' .. plugin_name)
        end
    end
end

load_plugin_configs()
