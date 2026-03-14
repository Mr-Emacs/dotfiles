local M = {}

M._index = {}

M._config = {
    ctags_program    = "ctags",
    debug            = false,
    ctags_extra_args = {},
    stdlib_headers   = {
        "stdio.h", "stdlib.h", "string.h", "math.h", "time.h",
        "stddef.h", "stdint.h", "stdbool.h", "stdarg.h",
        "ctype.h", "errno.h", "assert.h", "limits.h", "float.h",
        "signal.h", "setjmp.h", "locale.h",
    },
    stdlib_dirs      = {
        "/usr/include",
        "/usr/local/include",
    },
}

local function log(fmt, ...)
    if M._config.debug then
        vim.notify(string.format("[cgoogle] " .. fmt, ...), vim.log.levels.DEBUG)
    end
end

local function ctags_ok()
    local handle = io.popen(M._config.ctags_program .. " --version 2>&1")
    if not handle then return false end
    local out = handle:read("*a")
    handle:close()
    return out and out:find("Universal Ctags") ~= nil
end

local function ctags_argv(file, tmp)
    local args = {
        M._config.ctags_program,
        "-f", tmp,
        "--fields=+neKStzp",
        "--kinds-c=fp",
        "--extras=-F",
        "--language-force=C",
        "--output-format=json",
        "--sort=no",
    }
    for _, flag in ipairs(M._config.ctags_extra_args) do
        table.insert(args, "-D")
        table.insert(args, flag)
    end
    table.insert(args, file)
    return args
end

local function run_ctags(file)
    local tmp = vim.fn.tempname()
    local argv = ctags_argv(file, tmp)
    log("cmd: %s", table.concat(argv, " "))

    vim.fn.system(argv)
    local rc = vim.v.shell_error

    if rc ~= 0 then
        log("ctags exited %d on %s", rc, file)
        vim.fn.delete(tmp)
        return {}
    end

    local funcs = {}
    local f = io.open(tmp, "r")
    if f then
        for line in f:lines() do
            local entry = M._parse_json_line(line, file)
            if entry then
                table.insert(funcs, entry)
            end
        end
        f:close()
    end
    vim.fn.delete(tmp)
    log("parsed %d functions from %s", #funcs, vim.fn.fnamemodify(file, ":t"))
    return funcs
end

function M._parse_json_line(line, file)
    if not line or line:sub(1, 1) ~= "{" then return nil end
    local ok, data = pcall(vim.json.decode, line)
    if not ok or type(data) ~= "table" then return nil end

    local kind = data.kind
    if kind ~= "function" and kind ~= "prototype" then return nil end

    local name        = data.name or ""
    local line_num    = data.line or 1
    local signature   = data.signature or "()"
    local typeref     = data.typeref or ""

    local return_type = "void"
    if typeref ~= "" then
        local rt = typeref:match("^[^:]+:%s*(.+)$") or typeref
        rt = rt:gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", "")
        rt = rt:gsub("^RLAPI%s+", ""):gsub("^static%s+", ""):gsub("^inline%s+", ""):gsub("^extern%s+", "")
        return_type = (rt:gsub("^%s+", ""):gsub("%s+$", ""))
    else
        -- fallback: extract return type from the pattern field (raw source line)
        -- pattern looks like: /^Color ColorAlpha(Color color,float alpha);$/
        local pattern = data.pattern or ""
        local src = pattern:match("^/^%s*(.-)%s*/$") or pattern:match("^(.-)%s*/$") or ""
        -- strip leading storage qualifiers
        src = src:gsub("^%s*RLAPI%s+", ""):gsub("^%s*static%s+", ""):gsub("^%s*inline%s+", ""):gsub("^%s*extern%s+", "")
        -- everything before "name(" is the return type
        local rt = src:match("^(.-)%s*" .. vim.pesc(name) .. "%s*%(")
        if rt and rt ~= "" then
            return_type = (rt:gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", ""))
        end
    end

    local params  = M._parse_params(signature)
    local context = string.format("%s %s%s", return_type, name, signature)

    return {
        name        = name,
        file        = file,
        line        = line_num,
        signature   = signature,
        return_type = return_type,
        params      = params,
        context     = context,
    }
end

function M._parse_params(sig)
    if not sig then return {} end
    -- lenient: find first ( ... ) in the string, don't require exact anchors
    local inner = sig:match("%((.-)%)")
    if not inner then
        inner = sig:match("%((.+)$")
    end
    if not inner or inner:match("^%s*void%s*$") or inner:match("^%s*$") then
        return {}
    end
    local types = {}
    for part in inner:gmatch("[^,]+") do
        local extracted = M._extract_type(part)
        if extracted and extracted ~= "" then
            table.insert(types, extracted)
        end
    end
    return types
end

function M._extract_type(decl)
    local t = decl:gsub("^%s+", ""):gsub("%s+$", "")
    local typ = t:match("^(.-)%s+[%a_][%w_]*%s*%[?[%d%s]*%]?$")
    return (((typ or t):gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", "")))
end

function M._norm(t)
    if not t then return "" end
    local s = t
    for _, q in ipairs({ "const", "volatile", "restrict", "static", "inline", "extern" }) do
        s = s:gsub("%f[%a]" .. q .. "%f[%A]", "")
    end
    s = s:gsub("%s+", " ")
    s = s:gsub("%s*%*", "*")
    s = s:gsub("%*%s+", "*")
    return (s:gsub("^%s+", ""):gsub("%s+$", ""):lower())
end

function M._parse_query(query)
    local q = query:gsub("^%s+", ""):gsub("%s+$", "")

    if q:find("->") then
        local lhs, rhs = q:match("^(.-)%s*%->%s*(.+)$")
        local types = {}
        if lhs then
            if lhs:find(",") then
                for p in lhs:gmatch("[^,]+") do table.insert(types, M._norm(p)) end
            else
                for p in lhs:gmatch("%S+") do table.insert(types, M._norm(p)) end
            end
        end
        -- params_only=true: lhs types must match params only, not return type
        return { types = #types > 0 and types or nil, return_type = rhs and M._norm(rhs) or nil, params_only = true }
    end

    if q:find(",") then
        local types = {}
        for p in q:gmatch("[^,]+") do table.insert(types, M._norm(p)) end
        return { types = types }
    end

    local words = {}
    for w in q:gmatch("%S+") do table.insert(words, w) end

    if #words == 1 then
        return { name = words[1], types = { M._norm(words[1]) } }
    end

    local last  = words[#words]
    local other = table.concat(words, " ", 1, #words - 1)
    if last:match("^[a-z_]") then
        return { name = last, return_type = M._norm(other) }
    end

    local types = {}
    for _, w in ipairs(words) do table.insert(types, M._norm(w)) end
    return { types = types }
end

local function name_matches(qname, func)
    return func.name:lower():find(qname:lower(), 1, true) ~= nil
end

local function consume_match(qtypes, pool)
    local used = {} -- track consumed indices separately so ipairs stays intact
    for _, qt in ipairs(qtypes) do
        local idx = nil
        for i, ft in ipairs(pool) do
            if not used[i] and (ft:find(qt, 1, true) or qt:find(ft, 1, true) or ft:gsub("%*", "") == qt:gsub("%*", "")) then
                idx = i; break
            end
        end
        if not idx then return false end
        used[idx] = true
    end
    return true
end

local function func_matches(criteria, func)
    local qn  = criteria.name
    local qt  = criteria.types
    local qrt = criteria.return_type

    if qn and not qt then
        if not name_matches(qn, func) then return false end
        if qrt and not M._norm(func.return_type):find(qrt, 1, true) then return false end
        return true
    end

    if qn and qt and not qrt then
        if name_matches(qn, func) then return true end
        local pool = {}
        for _, p in ipairs(func.params) do table.insert(pool, M._norm(p)) end
        return consume_match(qt, pool)
    end

    if qt then
        local pool = {}
        for _, p in ipairs(func.params) do table.insert(pool, M._norm(p)) end
        if not consume_match(qt, pool) then return false end
    end
    if qrt and not M._norm(func.return_type):find(qrt, 1, true) then return false end
    return true
end

function M.search(query)
    if #M._index == 0 then
        vim.notify("cgoogle: index is empty — run :CgIndexFile or :CgIndexStdlib", vim.log.levels.WARN)
        return {}
    end
    local criteria = M._parse_query(query)
    log("criteria: %s", vim.inspect(criteria))
    local results = {}
    for _, func in ipairs(M._index) do
        if func_matches(criteria, func) then table.insert(results, func) end
    end
    return results
end

local function show_qf(query, results)
    local items = {}
    for _, func in ipairs(results) do
        table.insert(items, { filename = func.file, lnum = func.line, col = 1, text = func.context })
    end
    vim.fn.setqflist({}, "r", {
        title = string.format("cgoogle: %s (%d)", query, #results),
        items = items,
    })
    if #items == 0 then
        vim.notify(string.format("cgoogle: no matches for '%s'", query), vim.log.levels.WARN)
    else
        vim.cmd("copen")
        vim.cmd("cfirst")
        vim.notify(string.format("cgoogle: %d match%s — ]q/[q navigate, <CR> jump",
            #items, #items == 1 and "" or "es"), vim.log.levels.INFO)
    end
end

local function remove_file_entries(abs_file)
    local kept = {}
    for _, f in ipairs(M._index) do
        if f.file ~= abs_file then table.insert(kept, f) end
    end
    M._index = kept
end

function M.index_file(file)
    local abs = vim.fn.fnamemodify(vim.fn.expand(file), ":p")
    if vim.fn.filereadable(abs) == 0 then
        vim.notify("cgoogle: file not found: " .. abs, vim.log.levels.ERROR)
        return
    end
    remove_file_entries(abs)
    local funcs = run_ctags(abs)
    for _, f in ipairs(funcs) do table.insert(M._index, f) end
    vim.notify(string.format("cgoogle: indexed %d functions from %s (total %d)",
        #funcs, vim.fn.fnamemodify(abs, ":t"), #M._index), vim.log.levels.INFO)
end

function M.index_dir(dir)
    local abs = vim.fn.fnamemodify(vim.fn.expand(dir), ":p")
    local files = vim.fn.globpath(abs, "**/*.[ch]", false, true)
    if #files == 0 then
        vim.notify("cgoogle: no .c/.h files found under " .. abs, vim.log.levels.WARN)
        return
    end
    vim.notify(string.format("cgoogle: indexing %d files...", #files), vim.log.levels.INFO)
    local total = 0
    for _, f in ipairs(files) do
        remove_file_entries(f)
        local funcs = run_ctags(f)
        for _, fn in ipairs(funcs) do table.insert(M._index, fn) end
        total = total + #funcs
    end
    vim.notify(string.format("cgoogle: indexed %d functions from %d files (total %d)",
        total, #files, #M._index), vim.log.levels.INFO)
end

function M.index_stdlib()
    if not ctags_ok() then
        vim.notify("cgoogle: universal-ctags not found", vim.log.levels.ERROR)
        return
    end
    local dirs = vim.deepcopy(M._config.stdlib_dirs)
    if vim.fn.has("mac") == 1 then
        local sdk = vim.fn.trim(vim.fn.system("xcrun --show-sdk-path 2>/dev/null"))
        if sdk ~= "" then table.insert(dirs, sdk .. "/usr/include") end
    end
    local total, indexed = 0, 0
    for _, header in ipairs(M._config.stdlib_headers) do
        local found = false
        for _, d in ipairs(dirs) do
            local path = d .. "/" .. header
            if vim.fn.filereadable(path) == 1 then
                local funcs = run_ctags(path)
                for _, f in ipairs(funcs) do table.insert(M._index, f) end
                total   = total + #funcs
                indexed = indexed + 1
                found   = true
                break
            end
        end
        if not found then log("warning: could not find %s", header) end
    end
    vim.notify(string.format("cgoogle: stdlib quick-index done — %d functions from %d headers (total %d)",
        total, indexed, #M._index), vim.log.levels.INFO)
end

function M.search_prompt(query)
    if not query or query == "" then
        query = vim.fn.input("cgoogle search: ")
        if query == "" then return end
    end
    local results = M.search(query)
    show_qf(query, results)
end

function M._complete() return {} end

function M.setup(opts)
    if opts then
        M._config = vim.tbl_deep_extend("force", M._config, opts)
    end

    if not ctags_ok() then
        vim.notify(
            "cgoogle: universal-ctags not found\n"
            .. "  macOS: brew install universal-ctags\n"
            .. "  Linux: sudo apt install universal-ctags",
            vim.log.levels.WARN)
    end

    vim.api.nvim_create_user_command("CgIndexFile", function(a)
        M.index_file(a.args ~= "" and a.args or vim.fn.expand("%"))
    end, { nargs = "?", complete = "file", desc = "cgoogle: index a C/H file" })

    vim.api.nvim_create_user_command("CgIndexDir", function(a)
        M.index_dir(a.args ~= "" and a.args or vim.fn.getcwd())
    end, { nargs = "?", complete = "dir", desc = "cgoogle: index all .c/.h in a directory" })

    vim.api.nvim_create_user_command("CgIndexStdlib", function(_)
        M.index_stdlib()
    end, { desc = "cgoogle: fast-index essential stdlib headers" })

    vim.api.nvim_create_user_command("CgSearch", function(a)
        M.search_prompt(a.args ~= "" and a.args or nil)
    end, { nargs = "?", desc = "cgoogle: search by name / signature" })

    vim.api.nvim_create_user_command("CgSearchWord", function(_)
        local word = vim.fn.expand("<cword>")
        M.search_prompt(word ~= "" and word or nil)
    end, { desc = "cgoogle: search for word under cursor" })

    vim.api.nvim_create_user_command("CgStats", function(_)
        local seen, files = {}, {}
        for _, f in ipairs(M._index) do
            if not seen[f.file] then
                seen[f.file] = true; table.insert(files, f.file)
            end
        end
        vim.notify(string.format("cgoogle: %d functions across %d file(s) | ctags: %s",
            #M._index, #files, ctags_ok() and "ok" or "NOT FOUND"), vim.log.levels.INFO)
    end, { desc = "cgoogle: show index statistics" })

    vim.api.nvim_create_user_command("CgClear", function(_)
        M._index = {}
        vim.notify("cgoogle: index cleared", vim.log.levels.INFO)
    end, { desc = "cgoogle: wipe the function index" })

    vim.api.nvim_create_user_command("CgCheckCtags", function(_)
        if ctags_ok() then
            vim.notify("cgoogle: " .. vim.fn.trim(vim.fn.system(M._config.ctags_program .. " --version 2>&1")),
                vim.log.levels.INFO)
        else
            vim.notify("cgoogle: universal-ctags NOT found", vim.log.levels.ERROR)
        end
    end, { desc = "cgoogle: verify ctags installation" })

    vim.api.nvim_create_user_command("CgDebugFile", function(a)
        local file = vim.fn.fnamemodify(vim.fn.expand(a.args ~= "" and a.args or "%"), ":p")
        local tmp  = vim.fn.tempname()
        local argv = ctags_argv(file, tmp)
        vim.notify("cmd:\n" .. table.concat(argv, " "), vim.log.levels.INFO)
        vim.fn.system(argv)
        vim.notify("exit code: " .. vim.v.shell_error, vim.log.levels.INFO)
        local func_lines, total, shown = {}, 0, 0
        local f = io.open(tmp, "r")
        if f then
            for ln in f:lines() do
                if not ln:find('"_type"%s*:%s*"ptag"') then
                    total = total + 1
                    if shown < 5 then
                        table.insert(func_lines, ln)
                        shown = shown + 1
                    end
                end
            end
            f:close()
            vim.notify(string.format("function entries: %d (showing first 5):\n%s",
                total, table.concat(func_lines, "\n")), vim.log.levels.INFO)
            if func_lines[1] then
                local entry = M._parse_json_line(func_lines[1], file)
                vim.notify("parsed entry 1:\n" .. vim.inspect(entry), vim.log.levels.INFO)
            end
        else
            vim.notify("no output file produced", vim.log.levels.ERROR)
        end
        vim.fn.delete(tmp)
    end, { nargs = "?", complete = "file", desc = "cgoogle: debug ctags output" })
end

return M
