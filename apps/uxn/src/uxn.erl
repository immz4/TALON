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

-module(uxn).

-include("uxn.hrl").

-behaviour(gen_statem).

%% API
-export([start/0,stop/0,load/1,step/0,get_state/0]).

%% gen_statem callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([ready/3,running/3]).

%% API Functions
start() -> gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_statem:stop(?MODULE).

-spec load(Bytecode :: nonempty_binary()) -> term();
          (Filename :: string()) -> term().
load(Bytecode) when is_binary(Bytecode) -> gen_statem:call(?MODULE, {load_bytecode, Bytecode});
load(Filename) when is_list(Filename) -> gen_statem:call(?MODULE, {load_rom_file, Filename}).

-spec step() -> term().
step() -> gen_statem:call(?MODULE, step).

-spec get_state() -> term().
get_state() -> gen_statem:call(?MODULE, get_state).

%% Mandatory callback functions
init([]) ->
    process_flag(trap_exit, true),
    Uxn = #uxn_state{
        ram = #{},
        dev = #{},
        wst = #{dat => #{}, ptr => 0},
        rst = #{dat => #{}, ptr => 0}
    },
    % debugger:start(),
    {ok, ready, Uxn}.

callback_mode() -> state_functions.

terminate(_Reason, _State, _Data) -> ok.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

%% State: ready - waiting for bytecode to be loaded
-type load_type() ::
        {load_bytecode, nonempty_binary()}
      | {load_rom_file, nonempty_string()}.
-type ready_ret() :: 
        {next_state, running, uxn_state(), [gen_statem:reply_action()]}
      | {keep_state, uxn_state(), [gen_statem:reply_action()]}
      | {keep_state_and_data, [gen_statem:reply_action()]}.
-spec ready(Event :: {call, gen_statem:from()}, Msg :: load_type() | get_state, State :: uxn_state()) -> Result :: ready_ret().
ready({call, From}, {load_bytecode, Bytecode}, State) ->
    RomData = binary_to_list(Bytecode),
    NewRam = load_rom_into_ram(RomData, 16#100, State#uxn_state.ram),
    NewState = State#uxn_state{ram = NewRam},
    {next_state, running, NewState, [{reply, From, ok}]};
ready({call, From}, {load_rom_file, Filename}, State) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            RomData = binary_to_list(Binary),
            NewRam = load_rom_into_ram(RomData, 16#100, State#uxn_state.ram),
            NewState = State#uxn_state{ram = NewRam},
            {next_state, running, NewState, [{reply, From, ok}]};
        {error, Reason} ->
            {keep_state, State, [{reply, From, {error, Reason}}]}
    end;
ready({call, From}, get_state, State) -> {keep_state_and_data, [{reply, From, State}]};
ready({call, From}, _Msg, State) -> {keep_state, State, [{reply, From, {error, non_loaded}}]}.

%% State: running - executing UXN bytecode
-type step_ret() :: 
        {next_state, halted, uxn_state(), [gen_statem:reply_action()]}
      | {keep_state, uxn_state(), [gen_statem:reply_action()]}
      | {keep_state_and_data, [gen_statem:reply_action()]}.
-spec running(Event :: {call, gen_statem:from()}, step, uxn_state()) -> step_ret();
             (Event :: {call, gen_statem:from()}, get_state, uxn_state()) -> step_ret().
running({call, From}, step, State) ->
    PC = State#uxn_state.pc,
    Opcode = maps:get(PC, State#uxn_state.ram),
    <<ModeK:1, ModeR:1, Mode2:1, _Rest:5>> = <<Opcode>>,
    Stack = case ModeR of 
        0 -> wst;
        1 -> rst
    end,

    UpdatedState = State#uxn_state{pc = PC + 1, step_count = State#uxn_state.step_count + 1},

    case execute_opcode(Opcode, {ModeK, Mode2, Stack}, UpdatedState) of
        {halt, FinalState} -> {next_state, halted, FinalState, [{reply, From, {halt, FinalState}}]};
        {continue, FinalState} -> {keep_state, FinalState, [{reply, From, {continue, FinalState}}]};
        {error, wrong_opcode} -> {keep_state_and_data, [{reply, From, {error, wrong_opcode, Opcode}}]}
    end;
running({call, From}, get_state, State) -> {keep_state_and_data, [{reply, From, State}]}.

-spec load_rom_into_ram(Rom :: list(integer()), Offset :: pos_integer(), Ram :: map()) -> Ram :: map().
load_rom_into_ram([], _, Ram) -> Ram;
load_rom_into_ram([Byte | Rest], Offset, Ram) ->
    NewRam = Ram#{Offset => Byte},
    load_rom_into_ram(Rest, Offset + 1, NewRam).

%% Opcodes
-type execute_ret() ::
        {halt, State :: uxn_state()}
      | {continue, State :: uxn_state()}
      | {error, wrong_opcode}.
-spec execute_opcode(Opcode :: pos_integer(), Args :: args(), State :: uxn_state()) -> execute_ret().
execute_opcode(Opcode, Args, State) ->
    if
        Opcode == ?BRK  -> {halt, State};
        ?IS_LIT(Opcode) -> uxn_opcodes:lit(Opcode, State);
        ?IS_INC(Opcode) -> uxn_opcodes:inc(Args, State);
        ?IS_POP(Opcode) -> uxn_opcodes:pop(Args, State);
        ?IS_NIP(Opcode) -> uxn_opcodes:nip(Args, State);
        ?IS_SWP(Opcode) -> uxn_opcodes:swp(Args, State);
        ?IS_ROT(Opcode) -> uxn_opcodes:rot(Args, State);
        ?IS_DUP(Opcode) -> uxn_opcodes:dup(Args, State);
        ?IS_OVR(Opcode) -> uxn_opcodes:ovr(Args, State);
        ?IS_EQU(Opcode) -> uxn_opcodes:equ(Args, State);
        ?IS_NEQ(Opcode) -> uxn_opcodes:neq(Args, State);
        ?IS_GTH(Opcode) -> uxn_opcodes:gth(Args, State);
        ?IS_LTH(Opcode) -> uxn_opcodes:lth(Args, State);
        ?IS_JMP(Opcode) -> uxn_opcodes:jmp(Args, State);
        ?IS_JCN(Opcode) -> uxn_opcodes:jcn(Args, State);
        ?IS_JSR(Opcode) -> uxn_opcodes:jsr(Args, State);
        true            -> {error, wrong_opcode}
    end.
