require("urlview").setup({
    default_title = "Links:",
    default_picker = "telescope",
    default_prefix = "https://",
    default_action = "system",
    default_register = "+",
    default_include_branch = false,
    unique = true,
    sorted = true,
    log_level_min = vim.log.levels.INFO,
    jump = {
        prev = "[u",
        next = "]u",
    },
})
