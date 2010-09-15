#!/usr/bin/awk -f

## Adds contract checking to all function definitions in an Erlang
## file. (Kind of)

BEGIN {}
{sub(/\r/, "")} # Remove silly windows line endings
/^[a-z][a-zA-Z_0-9]*\(.*->/ { # Beginning of function
    acc = acc $0 "\n"
    command = gensub(/^([a-zA-Z0-9_]*)\((.*)\).*/, "\\1", $0)
    args    = gensub(/^([a-zA-Z0-9_]*)\((.*)\).*/, "\\2", $0)
    file = FILENAME
    sub(/\.erl/, "", file)
    acc = acc "    checker:run(" file "," command "_, [" args "]).\n\n"
    acc = acc command "_(" args ") ->\n"
    next
}

{acc = acc $0 "\n" }
END {
    print acc
}
