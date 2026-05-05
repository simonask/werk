; Comments
(comment) @comment

; ---- Keywords ---------------------------------------------------------------

; Top-level declaration keywords
"config" @keyword
"let" @keyword
"default" @keyword
"include" @keyword
"build" @keyword
"task" @keyword

; Recipe statement keywords
"run" @keyword
"spawn" @keyword
"from" @keyword
"to" @keyword
"depfile" @keyword
"copy" @keyword
"write" @keyword
"delete" @keyword
"touch" @keyword
"env" @keyword
"env-remove" @keyword
"match" @keyword
"filter-match" @keyword

; Log-level keywords
"info" @keyword
"warn" @keyword
"error" @keyword

; ---- Builtin functions -------------------------------------------------------

(builtin) @function.builtin

; ---- Operators ---------------------------------------------------------------

"|" @operator
"=>" @operator
"=" @operator

; ---- Punctuation -------------------------------------------------------------

["{" "}"] @punctuation.bracket
["[" "]"] @punctuation.bracket
["(" ")"] @punctuation.bracket
["<" ">"] @punctuation.bracket

"," @punctuation.delimiter

; ---- Strings ----------------------------------------------------------------

(string) @string
(string_content) @string
(escape_sequence) @string.escape

; Interpolations inside strings are highlighted as embedded / special content.
(string_interpolation) @string.special
(path_interpolation) @string.special
(interpolation_content) @string.special

; % is the pattern stem wildcard, e.g. "%.o" or "build/{%}.o"
(stem_wildcard) @string.special

; ---- Numbers ----------------------------------------------------------------

(integer) @number

; ---- Constants ---------------------------------------------------------------

(boolean) @constant.builtin

; ---- Variables ---------------------------------------------------------------

; Variable bindings
(let_statement name: (identifier) @variable)
(config_statement name: (identifier) @variable)

; Definition names
(task_definition name: (identifier) @function)
(build_definition pattern: (string) @string.special)

; Array subscript
(subscript_expression object: (identifier) @variable)
