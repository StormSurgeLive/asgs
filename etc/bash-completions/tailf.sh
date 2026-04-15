# START ASGS "tailf" completion
# This file is in the public domain

_tailf_completion()
{
    local cur prev
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "$prev" in
        syslog)
            # no further completion after subcommand
            return 0
            ;;
        *)
            COMPREPLY=( $(compgen -W "mail-log syslog" -- "$cur") )
            ;;
    esac

    return 0
}

complete -o default -F _tailf_completion tailf

# END tailf completion
