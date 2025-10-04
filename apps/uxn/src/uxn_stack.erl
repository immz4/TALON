%% Copyright (c) 2025 Zoe Lembrik
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
-export([read/3]).
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

%% Stack functions

-spec pop(Stack :: stack(),
          State :: uxn_state(),
          Amount :: pos_integer(),
          Values :: [pos_integer()]) ->
             {Values :: [pos_integer()], State :: uxn_state()}.
pop(_, State, 0, Values) -> {Values, State};
pop(wst, State, Amount, Values) ->
    Wst = State#uxn_state.wst,
    Ptr = maps:get(ptr, Wst),
    Dat = maps:get(dat, Wst),
    Value = maps:get(Ptr - 1, Dat),
    NewDat = maps:remove(Ptr - 1, Dat),
    NewWst = Wst#{dat => NewDat, ptr => Ptr - 1},
    pop(wst, State#uxn_state{wst = NewWst}, Amount - 1, [Value | Values]);
pop(rst, State, Amount, Values) ->
    Rst = State#uxn_state.rst,
    Ptr = maps:get(ptr, Rst),
    Dat = maps:get(dat, Rst),
    Value = maps:get(Ptr - 1, Dat),
    NewDat = maps:remove(Ptr - 1, Dat),
    NewRst = Rst#{dat => NewDat, ptr => Ptr - 1},
    pop(rst, State#uxn_state{rst = NewRst}, Amount - 1, [Value | Values]).

-spec pop(Stack :: stack(), State :: uxn_state(), Amount :: pos_integer()) ->
            {Values :: [pos_integer()], State :: uxn_state()}.
pop(Stack, State, Amount) -> pop(Stack, State, Amount, []).

-spec push(Stack :: stack(), Values :: [], State :: uxn_state()) -> uxn_state();
          (Stack :: stack(), Values :: list(integer()), State :: uxn_state()) -> uxn_state().
push(_, [], State) -> State;
push(wst, [Value | Rest], State) ->
    Wst = State#uxn_state.wst,
    Ptr = maps:get(ptr, Wst),
    Dat = maps:get(dat, Wst),
    NewDat = Dat#{Ptr => Value},
    NewWst = Wst#{dat => NewDat, ptr => Ptr + 1},
    push(wst, Rest, State#uxn_state{wst = NewWst});
push(rst, [Value | Rest], State) ->
    Rst = State#uxn_state.rst,
    Ptr = maps:get(ptr, Rst),
    Dat = maps:get(dat, Rst),
    NewDat = Dat#{Ptr => Value},
    NewRst = Rst#{dat => NewDat, ptr => Ptr + 1},
    push(rst, Rest, State#uxn_state{rst = NewRst}).

-spec get(Stack :: stack(),
          State :: uxn_state(),
          Offset :: integer(),
          Amount :: pos_integer(),
          Values :: [pos_integer()]) ->
             [pos_integer()].
get(_, _, _, 0, Values) -> Values;
get(wst, State, Offset, Amount, Values) ->
    Wst = State#uxn_state.wst,
    Ptr = maps:get(ptr, Wst),
    Dat = maps:get(dat, Wst),
    Value = maps:get(Ptr - Offset, Dat),
    get(wst, State, Offset - 1, Amount - 1, Values ++ [Value]);
get(rst, State, Offset, Amount, Values) ->
    Rst = State#uxn_state.rst,
    Ptr = maps:get(ptr, Rst),
    Dat = maps:get(dat, Rst),
    Value = maps:get(Ptr - Offset, Dat),
    get(rst, State, Offset - 1, Amount - 1, Values ++ [Value]).

-spec get(Stack :: stack(),
          State :: uxn_state(),
          Offset :: integer(),
          Amount :: pos_integer()) ->
             [pos_integer()].
get(Stack, State, Offset, Amount) -> get(Stack, State, Offset, Amount, []).

-spec get(Stack :: stack(), State :: uxn_state(), Offset :: integer()) -> list(pos_integer()).
get(Stack, State, Offset) -> get(Stack, State, Offset, 1, []).

-spec copy(Destination :: {ram, stack()}, State :: uxn_state(), Amount :: pos_integer()) -> uxn_state().
copy(_, State, 0) -> State;
copy({ram, wst}, State, Amount) ->
    PC = State#uxn_state.pc,
    Value = maps:get(PC, State#uxn_state.ram),
    UpdatedState = push(wst, [Value], State),
    copy({ram, wst}, UpdatedState#uxn_state{pc = PC + 1}, Amount - 1);
copy({ram, rst}, State, Amount) ->
    PC = State#uxn_state.pc,
    Value = maps:get(PC, State#uxn_state.ram),
    UpdatedState = push(rst, [Value], State),
    copy({ram, wst}, UpdatedState#uxn_state{pc = PC + 1}, Amount - 1).
