# Prompt
function describe_host {
    if [ "$SSH_CONNECTION" ]
    then
        echo "\h "
    fi
}

# This causes ZSH to be confused about the length of the prompt,
# which can lead to it removing preceding lines when it resets
# the prompt.
#
# function describe_cwd_title {
#    if [ ! $INSIDE_EMACS ]
#    then
#        echo -e "\033]0;${PWD##*/}\007"
#    fi
# }
 
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

setopt PROMPT_SUBST
PROMPT="\
%F{3}%T \
%F{1}\$(last_exit_prompt)\
%F{5}$(describe_host)\
%F{4}\$(aws_vault_prompt)\
%F{2}\$(virtual_env_prompt)\
\$(git_branch_prompt)\
%F{1}\$(git_dot)\
%F{6}\$(dirs) 
\$ %f"
