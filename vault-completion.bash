_VAULT_TAGS_CACHE_FILE=$HOME/.vault/tags.cache

_vault_tags() {
    if [ -e "$_VAULT_TAGS_CACHE_FILE" ]; then
        cat "$_VAULT_TAGS_CACHE_FILE"
    else
        vault list-tags
    fi
}

_vault() {
    local cur prev command commands
    COMPREPLY=()
    command="${COMP_WORDS[1]}"
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    commands="attach note list list-tags uri del search edit"

    if [ "$prev" = "-t" ] || [ "$command" = "list" -a "$prev" = "-T" ] ; then
        COMPREPLY=($(compgen -W "$(_vault_tags)" -- "$cur"))
        return 0
    elif [ "$prev" = "vault" ]; then
        COMPREPLY=($(compgen -W "$commands" -- "$cur"))
        return 0
    elif [ "$command" = attach ]; then
        compopt -o default
        COMPREPLY=()
        return 0
    else
        return 1
    fi
}

complete -F _vault vault
