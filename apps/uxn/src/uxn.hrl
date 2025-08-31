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

-record(uxn_state, {
    pc = 16#0100,                    % Program counter
    ram = #{},                       % RAM (64KB)
    dev = #{},                       % Device memory (256 bytes)
    wst = #{dat => #{}, ptr => 0},   % Working stack
    rst = #{dat => #{}, ptr => 0},   % Return stack
    console_vector = 0,              % Console interrupt vector
    step_count = 0                   % Step counter for debugging
}).

-type uxn_state() :: #uxn_state{}.
-type stack() :: wst | rst.
-type args() :: {ModeK :: integer(), Mode2 :: integer(), Stack :: stack()}.

%% Define MODULEDOC and DOC to handle module and function documentation.
-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

%% Define UXN opcode constants
-define(BRK, 16#00).

-define(INC, 16#01).
-define(INC2, 16#21).
-define(INCR, 16#41).
-define(INC2R, 16#61).
-define(INCK, 16#81).
-define(INC2K, 16#A1).
-define(INCKR, 16#C1).
-define(INC2KR, 16#E1).

-define(POP, 16#02).
-define(POP2, 16#22).
-define(POPR, 16#42).
-define(POP2R, 16#62).
-define(POPK, 16#82).
-define(POP2K, 16#A2).
-define(POPKR, 16#C2).
-define(POP2KR, 16#E2).

-define(NIP, 16#03).
-define(NIP2, 16#23).
-define(NIPR, 16#43).
-define(NIP2R, 16#63).
-define(NIPK, 16#83).
-define(NIP2K, 16#A3).
-define(NIPKR, 16#C3).
-define(NIP2KR, 16#E3).

-define(SWP, 16#04).
-define(SWP2, 16#24).
-define(SWPR, 16#44).
-define(SWP2R, 16#64).
-define(SWPK, 16#84).
-define(SWP2K, 16#A4).
-define(SWPKR, 16#C4).
-define(SWP2KR, 16#E4).

-define(ROT, 16#05).
-define(ROT2, 16#25).
-define(ROTR, 16#45).
-define(ROT2R, 16#65).
-define(ROTK, 16#85).
-define(ROT2K, 16#A5).
-define(ROTKR, 16#C5).
-define(ROT2KR, 16#E5).

-define(LIT, 16#80).
-define(LIT2, 16#A0).
-define(LITR, 16#C0).
-define(LIT2R, 16#E0).

%% Define helpers for working with opcodes
-define(IN(Op, List), lists:member(Op, List)).

-define(IS_INC(Op), Op == ?INC; Op == ?INC2; Op == ?INCR; Op == ?INC2R; Op == ?INCK; Op == ?INC2K; Op == ?INCKR; Op == ?INC2KR).
-define(IS_POP(Op), Op == ?POP; Op == ?POP2; Op == ?POPR; Op == ?POP2R; Op == ?POPK; Op == ?POP2K; Op == ?POPKR; Op == ?POP2KR).
-define(IS_NIP(Op), Op == ?NIP; Op == ?NIP2; Op == ?NIPR; Op == ?NIP2R; Op == ?NIPK; Op == ?NIP2K; Op == ?NIPKR; Op == ?NIP2KR).
-define(IS_SWP(Op), Op == ?SWP; Op == ?SWP2; Op == ?SWPR; Op == ?SWP2R; Op == ?SWPK; Op == ?SWP2K; Op == ?SWPKR; Op == ?SWP2KR).
-define(IS_ROT(Op), Op == ?ROT; Op == ?ROT2; Op == ?ROTR; Op == ?ROT2R; Op == ?ROTK; Op == ?ROT2K; Op == ?ROTKR; Op == ?ROT2KR).

-define(IS_LIT(Op), Op == ?LIT; Op == ?LIT2; Op == ?LITR; Op == ?LIT2R).