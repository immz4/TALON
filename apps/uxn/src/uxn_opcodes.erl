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


-module(uxn_opcodes).
-include("uxn.hrl").
-export([
    lit/2,
    inc/2,
    pop/2,
    nip/2,
    rot/2,
    swp/2,
    dup/2,
    ovr/2,
    equ/2,
    neq/2,
    gth/2,
    lth/2,
    jmp/2,
    jcn/2,
    jsr/2
]).

-spec lit(Opcode :: pos_integer(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
lit(Opcode, State) ->
    FinalState = case Opcode of
        16#80 -> uxn_stack:copy({ram, wst}, State, 1);
        16#A0 -> uxn_stack:copy({ram, wst}, State, 2);
        16#C0 -> uxn_stack:copy({ram, rst}, State, 1);
        16#E0 -> uxn_stack:copy({ram, rst}, State, 2)
    end,
    {continue, FinalState}.

-spec inc(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
inc(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Value], NewState} = uxn_stack:pop(Stack, State, 1),
            uxn_stack:push(Stack, [Value + 1], NewState);
        {1, 0, Stack} ->
            [Value] = uxn_stack:get(Stack, State, 1),
            uxn_stack:push(Stack, [Value + 1], State)
    end,
    {continue, FinalState}.

-spec pop(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
pop(Args, State) ->
    {_, FinalState} = case Args of
        {0, 0, Stack} -> uxn_stack:pop(Stack, State, 1);
        {0, 1, Stack} -> uxn_stack:pop(Stack, State, 2);
        {_, _, _} -> State
    end,
    {continue, FinalState}.

-spec nip(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
nip(Args, State) -> 
    FinalState = case Args of
        {0, 0, Stack} -> 
            {[Val1 | _], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            uxn_stack:push(Stack, [Val1], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2 | _], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            uxn_stack:push(Stack, [Val1, Val2], UpdatedState);
        {1, 0, Stack} ->
            Values = uxn_stack:get(Stack, State, 1),
            uxn_stack:push(Stack, Values, State);
        {1, 1, Stack} ->
            Values = uxn_stack:get(Stack, State, 4, 2),
            uxn_stack:push(Stack, Values, State)
    end,
    {continue, FinalState}.

-spec swp(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
swp(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2 | _], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            uxn_stack:push(Stack, [Val2, Val1], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2, Val3, Val4 | _], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            uxn_stack:push(Stack, [Val3, Val4, Val1, Val2], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            uxn_stack:push(Stack, [Val2, Val1], State);
        {1, 1, Stack} ->
            [Val4, Val3, Val2, Val1] = uxn_stack:get(Stack, State, 4, 4),
            uxn_stack:push(Stack, [Val2, Val1, Val4, Val3], State)
    end,
    {continue, FinalState}.

-spec rot(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
rot(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2, Val3 | _], UpdatedState} = uxn_stack:pop(Stack, State, 3),
            uxn_stack:push(Stack, [Val2, Val3, Val1], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2, Val3, Val4, Val5, Val6 | _], UpdatedState} = uxn_stack:pop(Stack, State, 6),
            uxn_stack:push(Stack, [Val3, Val4, Val5, Val6, Val1, Val2], UpdatedState);
        {1, 0, Stack} ->
            [Val3, Val2, Val1] = uxn_stack:get(Stack, State, 3, 3),
            uxn_stack:push(Stack, [Val2, Val1, Val3], State);
        {1, 1, Stack} ->
            [Val6, Val5, Val4, Val3, Val2, Val1] = uxn_stack:get(Stack, State, 6, 6),
            uxn_stack:push(Stack, [Val4, Val3, Val2, Val1, Val6, Val5], State)
    end,
    {continue, FinalState}.

-spec dup(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
dup(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1], UpdatedState} = uxn_stack:pop(Stack, State, 1),
            uxn_stack:push(Stack, [Val1, Val1], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            uxn_stack:push(Stack, [Val1, Val2, Val1, Val2], UpdatedState);
        {1, 0, Stack} ->
            [Val1] = uxn_stack:get(Stack, State, 1, 1),
            uxn_stack:push(Stack, [Val1, Val1], State);
        {1, 1, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            uxn_stack:push(Stack, [Val2, Val1, Val2, Val1], State)
    end,
    {continue, FinalState}.

-spec ovr(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
ovr(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            uxn_stack:push(Stack, [Val1, Val2, Val1], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2, Val3, Val4], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            uxn_stack:push(Stack, [Val1, Val2, Val3, Val4, Val1, Val2], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            uxn_stack:push(Stack, [Val2, Val1, Val2], State);
        {1, 1, Stack} ->
            [Val4, Val3, Val2, Val1] = uxn_stack:get(Stack, State, 4, 4),
            uxn_stack:push(Stack, [Val4, Val3, Val2, Val1, Val4, Val3], State)
    end,
    {continue, FinalState}.

-spec equ(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
equ(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            NewVal = uxn_stack:bool_to_num(Val1 == Val2),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2, Val3, Val4], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            NewVal = uxn_stack:bool_to_num((Val1 == Val3) and (Val2 == Val4)),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            NewVal = uxn_stack:bool_to_num(Val1 == Val2),        
            uxn_stack:push(Stack, [NewVal], State);
        {1, 1, Stack} ->
            [Val4, Val3, Val2, Val1] = uxn_stack:get(Stack, State, 4, 4),
            NewVal = uxn_stack:bool_to_num((Val1 == Val3) and (Val2 == Val4)),
            uxn_stack:push(Stack, [NewVal], State)
    end,
    {continue, FinalState}.

-spec neq(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
neq(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            NewVal = uxn_stack:bool_to_num(Val1 =/= Val2),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, Val2, Val3, Val4], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            NewVal = uxn_stack:bool_to_num((Val1 =/= Val3) or (Val2 =/= Val4)),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            NewVal = uxn_stack:bool_to_num(Val1 =/= Val2),        
            uxn_stack:push(Stack, [NewVal], State);
        {1, 1, Stack} ->
            [Val4, Val3, Val2, Val1] = uxn_stack:get(Stack, State, 4, 4),
            NewVal = uxn_stack:bool_to_num((Val1 =/= Val3) or (Val2 =/= Val4)),
            uxn_stack:push(Stack, [NewVal], State)
    end,
    {continue, FinalState}.

-spec gth(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
gth(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            NewVal = uxn_stack:bool_to_num(Val1 > Val2),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, _, Val3, _], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            NewVal = uxn_stack:bool_to_num(Val1 > Val3),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            NewVal = uxn_stack:bool_to_num(Val1 > Val2),        
            uxn_stack:push(Stack, [NewVal], State);
        {1, 1, Stack} ->
            [_, Val3, _, Val1] = uxn_stack:get(Stack, State, 4, 4),
            NewVal = uxn_stack:bool_to_num(Val1 > Val3),
            uxn_stack:push(Stack, [NewVal], State)
    end,
    {continue, FinalState}.

-spec lth(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
lth(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1, Val2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            NewVal = uxn_stack:bool_to_num(Val1 < Val2),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {0, 1, Stack} ->
            {[Val1, _, Val3, _], UpdatedState} = uxn_stack:pop(Stack, State, 4),
            NewVal = uxn_stack:bool_to_num(Val1 < Val3),
            uxn_stack:push(Stack, [NewVal], UpdatedState);
        {1, 0, Stack} ->
            [Val2, Val1] = uxn_stack:get(Stack, State, 2, 2),
            NewVal = uxn_stack:bool_to_num(Val1 < Val2),        
            uxn_stack:push(Stack, [NewVal], State);
        {1, 1, Stack} ->
            [_, Val3, _, Val1] = uxn_stack:get(Stack, State, 4, 4),
            NewVal = uxn_stack:bool_to_num(Val1 < Val3),
            uxn_stack:push(Stack, [NewVal], State)
    end,
    {continue, FinalState}.

-spec jmp(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
jmp(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1], UpdatedState} = uxn_stack:pop(Stack, State, 1),
            UpdatedState#uxn_state{pc = State#uxn_state.pc + Val1};
        {0, 1, Stack} ->
            {[Byte1, Byte2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            UpdatedState#uxn_state{pc = Address};
        {1, 0, Stack} ->
            [Val1] = uxn_stack:get(Stack, State, 1, 1),
            State#uxn_state{pc = State#uxn_state.pc + Val1};
        {1, 1, Stack} ->
            [Byte2, Byte1] = uxn_stack:get(Stack, State, 2, 2),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            State#uxn_state{pc = Address}
    end,
    {continue, FinalState}.

-spec jcn(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
jcn(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Cond, Val1], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            case Cond > 0 of
                true -> UpdatedState#uxn_state{pc = State#uxn_state.pc + Val1};
                false -> UpdatedState
            end;
        {0, 1, Stack} ->
            {[Cond, Byte1, Byte2], UpdatedState} = uxn_stack:pop(Stack, State, 3),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            case Cond > 0 of
                true -> UpdatedState#uxn_state{pc = Address};
                false -> UpdatedState
            end;
        {1, 0, Stack} ->
            [Val1, Cond] = uxn_stack:get(Stack, State, 2, 2),
            case Cond > 0 of
                true -> State#uxn_state{pc = State#uxn_state.pc + Val1};
                false -> State
            end;
        {1, 1, Stack} ->
            [Byte2, Byte1, Cond] = uxn_stack:get(Stack, State, 3, 3),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            case Cond > 0 of
                true -> State#uxn_state{pc = Address};
                false -> State
            end
    end,
    {continue, FinalState}.

-spec jsr(Args :: args(), State :: uxn_state()) -> {continue, State :: uxn_state()}.
jsr(Args, State) ->
    FinalState = case Args of
        {0, 0, Stack} ->
            {[Val1], UpdatedState} = uxn_stack:pop(Stack, State, 1),
            uxn_stack:push(
                Stack,
                [State#uxn_state.pc],
          UpdatedState#uxn_state{pc = State#uxn_state.pc + Val1}
            );
        {0, 1, Stack} ->
            {[Byte1, Byte2], UpdatedState} = uxn_stack:pop(Stack, State, 2),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            uxn_stack:push(
                Stack,
                [State#uxn_state.pc],
          UpdatedState#uxn_state{pc = Address}
            );
        {1, 0, Stack} ->
            [Val1] = uxn_stack:get(Stack, State, 1, 1),
            uxn_stack:push(
                Stack,
                [State#uxn_state.pc],
          State#uxn_state{pc = State#uxn_state.pc + Val1}
            );
        {1, 1, Stack} ->
            [Byte2, Byte1] = uxn_stack:get(Stack, State, 2, 2),
            Address = uxn_stack:byte_to_short({Byte1, Byte2}),
            uxn_stack:push(
                Stack,
                [State#uxn_state.pc],
          State#uxn_state{pc = Address}
            )
    end,
    {continue, FinalState}.
