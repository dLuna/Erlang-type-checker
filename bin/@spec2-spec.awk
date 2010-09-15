#!/usr/bin/awk -f

## This script takes an Erlang file with @spec declarations and
## converts them into -spec declarations.  It works decently well if
## the declarations are reasonably simple.

BEGIN {FS = " "}
{sub(/\r/, "")} # Remove silly windows line endings
# This is the beginning of a @spec
/@spec / {
    acc = acc $0 "\n"
    sub(/%%*.*@spec */, ""); sub(/\. */, "") ## Remove @spec and maybe "."
    current = "-spec " $0                    ## Add -spec and set in_spec flag
    in_spec=1
    next
}
# Contains an = so if currently parsing a spec we stop that and
# instead do replacement of the type variables.
/.*[a-zA-Z][a-zA-Z]* *= *.*/ {
    if (in_spec) {
	acc = acc $0 "\n"
	sub(/%%* */, "")                     ## remove comment characters
	if (/->/) {type = ("fun(" $3 $4 $5 $6 $7 $8 $9 ")")
	    gsub(/[A-Z][a-z0-9A-Z]*/, "_", type)}
	else {type = $3 $4 $5 $6 $7 $8 $9}
	if (current ~ "::"$1) {
	    gsub($1, type, current)          ## replace type var with type
	} else
	{
	    gsub($1, $1 "::" type, current)  ## replace variable with type
	}
	end = 1                              ## flag!
	next
    }
}
# Second (and third etc) line of a spec
/^[^-a-z0-9_A-Z]*%[^@=]*$/ {
    if (in_spec && ! end) {
	acc = acc $0 "\n"
	sub(/%%* */, "")
	sub(/\. *$/, "")
	current = current $0
	next
    }
}
/^%% *$/ { if(in_spec) {current = current $0 "\n"}}
in_spec { # Finished parsing a spec so add the finishing . and concat
    current = current ".\n"
    current = gensub(/([^:])\<([A-Z][a-zA-Z0-9]*) *([^:A-Za-z ])/,
		     "\\1\\2::\\2\\3", "g", current)
    part = current
    a = ""
    while (match(part, /::[A-Z][A-Za-z0-9_]*/)) {
	a = a substr(part, 1, RSTART-1)
	a = a tolower(substr(part, RSTART, RLENGTH)) "()"
	part = substr(part, RSTART+RLENGTH)
    }
    current = a part
    acc = acc current
}
{ # All other lines. Just add them. Reset the vars.
    acc = acc $0 "\n"
    in_spec = 0; end = 0
}
END { # Find the end of the last export statement...
    while (match(acc, /\n-export\([^)]*\)\./)) {
	print substr(acc, 1, RSTART + RLENGTH)
	acc = substr(acc, RSTART + RLENGTH)
    } # ... add type declarations ...
    types["boolean"]              = "t | f"
    types["reason"]               = "any()"
    types["moduleinfo"]           = "list()"
    types["module_info"]          = "list()"
    types["arity"]                = "integer()"
    types["functionname"]         = "atom()"
    types["filename"]             = "atom() | string()"
    types["term"]                 = "any()"
    types["syntaxTree"]           = "any()"
    types["expression"]           = "erl_parse()"
    types["pattern"]              = "erl_parse()"
    types["erl_parse"]            = "any()"
    types["syntaxTreeAttributes"] = "list()"
    types["vars"]                 = "[atom()]"
    types["key"]                  = "env | bound | free"
    types["clause"]               = "{clause, _, _, _, _}"
    types["pos"]                  = "{integer(), integer()}"
    types["var"]                  = "atom()"
    for (a in types) # (but only if needed)
	if ((typedecl acc) ~ a "\\(\\)")
#	    acc = "-type " a "() :: " types[a] ".\n" acc
	    acc = "-type " a "() :: " "any()" ".\n" acc
      # ... and then print the rest.
    acc = typedecl acc
    print acc
}
