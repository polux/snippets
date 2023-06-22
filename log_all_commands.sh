# Copyright 2023 Google LLC.
# SPDX-License-Identifier: Apache-2.0

function prompt_command
{
  history -a
  echo \"$(date --rfc-3339=second)\" \"$(tail -n 1 ~/.bash_history)\" >> $HOME/commandlog.txt
}

export PROMPT_COMMAND=prompt_command

