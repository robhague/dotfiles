# Utilities
function source_if_present {
    if [ -f $1 ]
    then
        source $1
    fi
}

function pathadd {
    PATH=:$PATH
    PATH=$1${PATH//:$1/}
}

function envpass {
    read -s $1
    export $1
}

# General completion

function _ssh {
    COMPREPLY=( $(compgen -W "$( grep "^Host" ~/.ssh/config | grep -v '[?*]' | cut -d ' ' -f 2- )" -- $2 ) );
}
complete -F _ssh ssh

# Prompt
function describe_host {
    if [ "$SSH_CONNECTION" ]
    then
        echo "\h "
    fi
}

function describe_cwd_title {
    if [ ! $INSIDE_EMACS ]
    then
        echo -e "\033]0;${PWD##*/}\007"
    fi
}
function last_exit_prompt {
    STATUS=$?
    if [[ $STATUS > 0 ]]
    then
        echo "!$STATUS "
    fi
}
function git_branch_prompt {
    git branch --no-color 2> /dev/null | \
	sed -e '/^[^*]/d' -e 's/* \(.*\)/git:\1 /'
}

function git_dot {
    if [ "$(git status --porcelain 2> /dev/null)" ]
    then
        echo '● '
    fi
}

function virtual_env_prompt {
    if [ $PIPENV_ACTIVE ]
    then
       echo "PipEnv "
    elif [ $VIRTUAL_ENV ]
    then
        local BASENAME=$(basename $VIRTUAL_ENV)
        echo "Py:$BASENAME "
    fi
}

function aws_vault_prompt {
    if [ $AWS_VAULT ]
    then
       echo "AWS:$AWS_VAULT "
    fi
}

PS1="\
\[\e[33m\]\t \
\[\e[31m\]\$(last_exit_prompt)\
\[\e[35m\]$(describe_host)\
\[\e[34m\]\$(aws_vault_prompt)\
\[\e[32m\]\$(virtual_env_prompt)\
\$(git_branch_prompt)\
\[\e[31m\]\$(git_dot)\
\[\e[36m\]\$(dirs) \
\$(describe_cwd_title)\
\n\$ \[\e[m\]"

export PS1
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Command Completion
source_if_present ~/.git-completion.bash

# Disable El Capitan per-session history
export SHELL_SESSION_HISTORY=0

# Skip commands prefixed with a space
export HISTCONTROL=ignorespace

# Path
pathadd ~/scripts
pathadd ~/bin
pathadd ./node_modules/.bin/
export PATH

# Source local config
source_if_present ~/.bashrc.local
