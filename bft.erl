% Copyright 2016 Google Inc. All Rights Reserved.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(bft).
-compile([debug_info, export_all]).

loyal_lieutenant(MainPid) ->
  receive
    {all_pids, Pids} ->
       MainPid ! {self(), {here_is_my_value, 42}}
  end.

main() ->
  Pids = [ spawn(?MODULE, loyal_lieutenant, [self()]) || _ <- lists:seq(1,4) ],
  lists:foreach(fun (Pid) -> Pid ! {all_pids, Pids} end, Pids),
  Responses = [ receive {Pid, {here_is_my_value, Value}} -> {Pid, Value} end || _ <- lists:seq(1, 4) ],
  io:format("responses: ~p", [Responses]).
