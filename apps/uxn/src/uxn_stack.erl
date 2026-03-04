%% Copyright (c) 2026 Zoe Lembrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(uxn_stack).
-include("uxn.hrl").
-export([pop/3, push/3, get/3, get/4, copy/3]).
-export([byte_to_short/1, short_to_byte/1, bool_to_num/1, opposite/1]).

%% Utility functions

-spec byte_to_short({ByteHigh :: integer(), ByteLow :: integer()}) -> integer().
byte_to_short({ByteHigh, ByteLow}) -> (ByteHigh bsl 8) bor ByteLow.

-spec short_to_byte(Short :: integer()) -> {ByteHigh :: integer(), ByteLow :: integer()}.
short_to_byte(Short) -> {Short bsr 8, Short band 16#FF}.

-spec bool_to_num(Boolean :: boolean()) -> 1 | 0.
bool_to_num(true) -> 1;
bool_to_num(false) -> 0.

-spec opposite(wst) -> rst;
              (rst) -> wst.
opposite(wst) -> rst;
opposite(rst) -> wst.

-spec slice(Arr :: array:array(integer()), Last :: pos_integer()) -> [integer()].
-spec slice(Arr :: array:array(integer()), Start :: pos_integer(), Last :: pos_integer()) -> [integer()].
slice(Arr, Last) -> do_slice(Arr, 0, Last, []).
slice(Arr, Start, Last) -> do_slice(Arr, Start, Last, []).

-compile({inline, [do_slice/4]}).
-spec do_slice(
    Arr :: array:array(integer()),
    Start :: pos_integer(),
    Last :: pos_integer(),
    Acc :: [integer()]
) -> [integer()].
do_slice(_arr, I, I, Acc) -> Acc;
do_slice(Arr, Start, I, Acc) ->
    New_acc = [array:get(I, Arr) | Acc],
    do_slice(Arr, Start, I - 1, New_acc).

-spec get_stack(stack(), uxn_state()) -> {array:array(integer()), non_neg_integer()}.
get_stack(wst, #uxn_state{wst = {Dat, Ptr}}) -> {Dat, Ptr};
get_stack(rst, #uxn_state{rst = {Dat, Ptr}}) -> {Dat, Ptr}.

-spec set_stack(stack(), array:array(integer()), non_neg_integer(), uxn_state()) -> uxn_state().
set_stack(wst, Dat, Ptr, State) -> State#uxn_state{wst = {Dat, Ptr}};
set_stack(rst, Dat, Ptr, State) -> State#uxn_state{rst = {Dat, Ptr}}.

%% Stack functions

-spec pop(Stack :: stack(), State :: uxn_state(), Amount :: pos_integer()) ->
            {Values :: [pos_integer()], State :: uxn_state()}.
pop(Stack, State, Amount) -> do_pop(Stack, State, Amount, []).

-spec do_pop(Stack :: stack(),
          State :: uxn_state(),
          Amount :: pos_integer(),
          Values :: [pos_integer()]) ->
             {Values :: [pos_integer()], State :: uxn_state()}.
do_pop(_, State, 0, Values) -> {Values, State};
do_pop(Stack, State, Amount, Values) ->
    {Dat, Ptr} = get_stack(Stack, State),
    Value = array:get(Ptr - 1, Dat),
    NewDat = array:set(Ptr - 1, 0, Dat),
    NewState = set_stack(Stack, NewDat, Ptr - 1, State),
    do_pop(wst, NewState, Amount - 1, [Value | Values]).

-spec push(Stack :: stack(), Values :: [], State :: uxn_state()) -> uxn_state();
          (Stack :: stack(), Values :: list(integer()), State :: uxn_state()) -> uxn_state().
push(_, [], State) -> State;
push(Stack, [Value | Rest], State) ->
    {Dat, Ptr} = get_stack(Stack, State),
    NewDat = array:set(Ptr, Value, Dat),
    NewState = set_stack(Stack, NewDat, Ptr + 1, State),
    push(wst, Rest, NewState).

-spec get(Stack :: stack(),
          State :: uxn_state(),
          Offset :: integer(),
          Amount :: pos_integer()) ->
             [pos_integer()].
get(Stack, State, Offset, Amount) -> do_get(Stack, State, Offset, Amount, []).

-spec get(Stack :: stack(), State :: uxn_state(), Offset :: integer()) -> list(pos_integer()).
get(Stack, State, Offset) -> do_get(Stack, State, Offset, 1, []).

-spec do_get(Stack :: stack(),
          State :: uxn_state(),
          Offset :: integer(),
          Amount :: pos_integer(),
          Values :: [pos_integer()]) ->
             [pos_integer()].
do_get(_, _, _, 0, Values) -> Values;
do_get(Stack, State, Offset, Amount, Values) ->
    {Dat, Ptr} = get_stack(Stack, State),
    Value = array:get(Ptr - Offset, Dat),
    do_get(Stack, State, Offset - 1, Amount - 1, Values ++ [Value]).

-spec copy(Destination :: {ram, stack()}, State :: uxn_state(), Amount :: pos_integer()) -> uxn_state().
copy(_, State, 0) -> State;
copy({ram, Stack}, State, Amount) ->
    PC = State#uxn_state.pc,
    Value = array:get(PC, State#uxn_state.ram),
    UpdatedState = push(Stack, [Value], State),
    copy({ram, Stack}, UpdatedState#uxn_state{pc = PC + 1}, Amount - 1).
