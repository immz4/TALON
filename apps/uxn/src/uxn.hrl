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

-define(DUP, 16#06).
-define(DUP2, 16#26).
-define(DUPR, 16#46).
-define(DUP2R, 16#66).
-define(DUPK, 16#86).
-define(DUP2K, 16#A6).
-define(DUPKR, 16#C6).
-define(DUP2KR, 16#E6).

-define(OVR, 16#07).
-define(OVR2, 16#27).
-define(OVRR, 16#47).
-define(OVR2R, 16#67).
-define(OVRK, 16#87).
-define(OVR2K, 16#A7).
-define(OVRKR, 16#C7).
-define(OVR2KR, 16#E7).

-define(EQU, 16#08).
-define(EQU2, 16#28).
-define(EQUR, 16#48).
-define(EQU2R, 16#68).
-define(EQUK, 16#88).
-define(EQU2K, 16#A8).
-define(EQUKR, 16#C8).
-define(EQU2KR, 16#E8).

-define(NEQ, 16#09).
-define(NEQ2, 16#29).
-define(NEQR, 16#49).
-define(NEQ2R, 16#69).
-define(NEQK, 16#89).
-define(NEQ2K, 16#A9).
-define(NEQKR, 16#C9).
-define(NEQ2KR, 16#E9).

-define(GTH, 16#0A).
-define(GTH2, 16#2A).
-define(GTHR, 16#4A).
-define(GTH2R, 16#6A).
-define(GTHK, 16#8A).
-define(GTH2K, 16#AA).
-define(GTHKR, 16#CA).
-define(GTH2KR, 16#EA).

-define(LTH, 16#0B).
-define(LTH2, 16#2B).
-define(LTHR, 16#4B).
-define(LTH2R, 16#6B).
-define(LTHK, 16#8B).
-define(LTH2K, 16#AB).
-define(LTHKR, 16#CB).
-define(LTH2KR, 16#EB).

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
-define(IS_DUP(Op), Op == ?DUP; Op == ?DUP2; Op == ?DUPR; Op == ?DUP2R; Op == ?DUPK; Op == ?DUP2K; Op == ?DUPKR; Op == ?DUP2KR).
-define(IS_OVR(Op), Op == ?OVR; Op == ?OVR2; Op == ?OVRR; Op == ?OVR2R; Op == ?OVRK; Op == ?OVR2K; Op == ?OVRKR; Op == ?OVR2KR).
-define(IS_EQU(Op), Op == ?EQU; Op == ?EQU2; Op == ?EQUR; Op == ?EQU2R; Op == ?EQUK; Op == ?EQU2K; Op == ?EQUKR; Op == ?EQU2KR).
-define(IS_NEQ(Op), Op == ?NEQ; Op == ?NEQ2; Op == ?NEQR; Op == ?NEQ2R; Op == ?NEQK; Op == ?NEQ2K; Op == ?NEQKR; Op == ?NEQ2KR).
-define(IS_GTH(Op), Op == ?GTH; Op == ?GTH2; Op == ?GTHR; Op == ?GTH2R; Op == ?GTHK; Op == ?GTH2K; Op == ?GTHKR; Op == ?GTH2KR).
-define(IS_LTH(Op), Op == ?LTH; Op == ?LTH2; Op == ?LTHR; Op == ?LTH2R; Op == ?LTHK; Op == ?LTH2K; Op == ?LTHKR; Op == ?LTH2KR).

-define(IS_LIT(Op), Op == ?LIT; Op == ?LIT2; Op == ?LITR; Op == ?LIT2R).