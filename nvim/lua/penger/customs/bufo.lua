local M = {}

function M.setup()
    vim.api.nvim_create_augroup("bufo_detect", { clear = true })

    vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        group   = "bufo_detect",
        pattern = "*.bufo",
        callback = function()
            vim.bo.filetype = "bufo"
        end,
    })

    vim.api.nvim_create_autocmd("FileType", {
        group   = "bufo_detect",
        pattern = "bufo",
        callback = function()
            vim.schedule(function()
                vim.treesitter.stop()

                local function syn(type, name, pattern)
                    vim.cmd(string.format("syn %s %s %s", type, name, pattern))
                end

                syn("match",   "bufoIdentifier",  [["_*[a-z][a-zA-Z0-9_]*"]])
                syn("match",   "bufoUserType",     [["_*[A-Z][a-zA-Z0-9_]\+"]])
                syn("match",   "bufoFunction",     [["\<[A-Za-z_][A-Za-z0-9_]*\>\s*\ze("]])
                syn("match",   "bufoBuiltinFn",    [["@_*[a-zA-Z0-9_]*"]])
                syn("match",   "bufoNumber",       [["\v<[0-9](_*[0-9])*([iu](8|16|32|64)|([Uu][Ll]?|[Ll]))?>"]])
                syn("match",   "bufoOperator",     [["\v(\<\<|\>\>|[<>=!+*/%&~^|-])\=?"]])
                syn("match",   "bufoDelimiter",    [["\v[;,:\{\}\(\)\[\].?]"]])
                syn("match",   "bufoString",       [["\"(\\.|[^\\\"])*\""]])
                syn("match",   "bufoString",       [["`(``|.)*`"]])
                syn("region",  "bufoComment",      [[start="/\*" end="\*/" contains=bufoComment,bufoTodo]])
                syn("region",  "bufoComment",      [[start="//" end="$" contains=bufoTodo]])
                syn("keyword", "bufoTodo",         "TODO FIXME REVIEW contained")
                syn("keyword", "bufoKeyword",      "if else match return struct func union module let comptime asm import as sizeof type_info")
                syn("keyword", "bufoRepeat",       "while for continue break")
                syn("keyword", "bufoBuiltinType",  "Any bool char i8 i16 i32 i64 u8 u16 u32 u64 usize f32 f64")
                syn("keyword", "bufoBuiltinIdent", "true false this null blank")

                local links = {
                    bufoBuiltinIdent = "Include",
                    bufoBuiltinType  = "Type",
                    bufoBuiltinFn    = "Title",
                    bufoNumber       = "Number",
                    bufoIdentifier   = "Identifier",
                    bufoFunction     = "Function",
                    bufoUserType     = "Type",
                    bufoKeyword      = "Keyword",
                    bufoRepeat       = "Repeat",
                    bufoString       = "String",
                    bufoOperator     = "Operator",
                    bufoDelimiter    = "Delimiter",
                    bufoComment      = "Comment",
                    bufoTodo         = "Todo",
                }

                for from, to in pairs(links) do
                    vim.cmd(string.format("hi def link %s %s", from, to))
                end

                vim.b.current_syntax = "bufo"
            end)
        end,
    })
end

return M
