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
-define(UXN_OPCS, #{
    16#00 => op_brk,
    16#20 => op_jci,
    16#40 => op_jmi,
    16#60 => op_jsi,
    16#80 => op_lit,
    16#A0 => op_lit,
    16#C0 => op_lit,
    16#E0 => op_lit,

    16#10 => op_ldz,
    16#30 => op_ldz,
    16#50 => op_ldz,
    16#70 => op_ldz,
    16#90 => op_ldz,
    16#B0 => op_ldz,
    16#D0 => op_ldz,
    16#F0 => op_ldz,

    16#01 => op_inc, 16#11 => op_stz, 
    16#21 => op_inc, 16#31 => op_stz,
    16#41 => op_inc, 16#51 => op_stz,
    16#61 => op_inc, 16#71 => op_stz,
    16#81 => op_inc, 16#91 => op_stz,
    16#A1 => op_inc, 16#B1 => op_stz,
    16#C1 => op_inc, 16#D1 => op_stz,
    16#E1 => op_inc, 16#F1 => op_stz,

    16#02 => op_pop, 16#12 => op_ldr,
    16#22 => op_pop, 16#32 => op_ldr,
    16#42 => op_pop, 16#52 => op_ldr,
    16#62 => op_pop, 16#72 => op_ldr,
    16#82 => op_pop, 16#92 => op_ldr,
    16#A2 => op_pop, 16#B2 => op_ldr,
    16#C2 => op_pop, 16#D2 => op_ldr,
    16#E2 => op_pop, 16#F2 => op_ldr,

    16#03 => op_nip, 16#13 => op_str,
    16#23 => op_nip, 16#33 => op_str,
    16#43 => op_nip, 16#53 => op_str,
    16#63 => op_nip, 16#73 => op_str,
    16#83 => op_nip, 16#93 => op_str,
    16#A3 => op_nip, 16#B3 => op_str,
    16#C3 => op_nip, 16#D3 => op_str,
    16#E3 => op_nip, 16#F3 => op_str,

    16#04 => op_swp, 16#14 => op_lda,
    16#24 => op_swp, 16#34 => op_lda,
    16#44 => op_swp, 16#54 => op_lda,
    16#64 => op_swp, 16#74 => op_lda,
    16#84 => op_swp, 16#94 => op_lda,
    16#A4 => op_swp, 16#B4 => op_lda,
    16#C4 => op_swp, 16#D4 => op_lda,
    16#E4 => op_swp, 16#F4 => op_lda,

    16#05 => op_rot, 16#15 => op_sta,
    16#25 => op_rot, 16#35 => op_sta,
    16#45 => op_rot, 16#55 => op_sta,
    16#65 => op_rot, 16#75 => op_sta,
    16#85 => op_rot, 16#95 => op_sta,
    16#A5 => op_rot, 16#B5 => op_sta,
    16#C5 => op_rot, 16#D5 => op_sta,
    16#E5 => op_rot, 16#F5 => op_sta,

    16#06 => op_dup, 16#16 => op_dei,
    16#26 => op_dup, 16#36 => op_dei,
    16#46 => op_dup, 16#56 => op_dei,
    16#66 => op_dup, 16#76 => op_dei,
    16#86 => op_dup, 16#96 => op_dei,
    16#A6 => op_dup, 16#B6 => op_dei,
    16#C6 => op_dup, 16#D6 => op_dei,
    16#E6 => op_dup, 16#F6 => op_dei,

    16#07 => op_ovr, 16#17 => op_deo,
    16#27 => op_ovr, 16#37 => op_deo,
    16#47 => op_ovr, 16#57 => op_deo,
    16#67 => op_ovr, 16#77 => op_deo,
    16#87 => op_ovr, 16#97 => op_deo,
    16#A7 => op_ovr, 16#B7 => op_deo,
    16#C7 => op_ovr, 16#D7 => op_deo,
    16#E7 => op_ovr, 16#F7 => op_deo,
   
    16#08 => op_equ, 16#18 => op_add,
    16#28 => op_equ, 16#38 => op_add,
    16#48 => op_equ, 16#58 => op_add,
    16#68 => op_equ, 16#78 => op_add,
    16#88 => op_equ, 16#98 => op_add,
    16#A8 => op_equ, 16#B8 => op_add,
    16#C8 => op_equ, 16#D8 => op_add,
    16#E8 => op_equ, 16#F8 => op_add,

    16#09 => op_neq, 16#19 => op_sub,
    16#29 => op_neq, 16#39 => op_sub,
    16#49 => op_neq, 16#59 => op_sub,
    16#69 => op_neq, 16#79 => op_sub,
    16#89 => op_neq, 16#99 => op_sub,
    16#A9 => op_neq, 16#B9 => op_sub,
    16#C9 => op_neq, 16#D9 => op_sub,
    16#E9 => op_neq, 16#F9 => op_sub,

    16#0A => op_gth, 16#1A => op_mul,
    16#2A => op_gth, 16#3A => op_mul,
    16#4A => op_gth, 16#5A => op_mul,
    16#6A => op_gth, 16#7A => op_mul,
    16#8A => op_gth, 16#9A => op_mul,
    16#AA => op_gth, 16#BA => op_mul,
    16#CA => op_gth, 16#DA => op_mul,
    16#EA => op_gth, 16#FA => op_mul,

    16#0B => op_lth, 16#1B => op_div,
    16#2B => op_lth, 16#3B => op_div,
    16#4B => op_lth, 16#5B => op_div,
    16#6B => op_lth, 16#7B => op_div,
    16#8B => op_lth, 16#9B => op_div,
    16#AB => op_lth, 16#BB => op_div,
    16#CB => op_lth, 16#DB => op_div,
    16#EB => op_lth, 16#FB => op_div,

    16#0C => op_jmp, 16#1C => op_and,
    16#2C => op_jmp, 16#3C => op_and,
    16#4C => op_jmp, 16#5C => op_and,
    16#6C => op_jmp, 16#7C => op_and,
    16#8C => op_jmp, 16#9C => op_and,
    16#AC => op_jmp, 16#BC => op_and,
    16#CC => op_jmp, 16#DC => op_and,
    16#EC => op_jmp, 16#FC => op_and,

    16#0D => op_jcn, 16#1D => op_ora,
    16#2D => op_jcn, 16#3D => op_ora,
    16#4D => op_jcn, 16#5D => op_ora,
    16#6D => op_jcn, 16#7D => op_ora,
    16#8D => op_jcn, 16#9D => op_ora,
    16#AD => op_jcn, 16#BD => op_ora,
    16#CD => op_jcn, 16#DD => op_ora,
    16#ED => op_jcn, 16#FD => op_ora,

    16#0E => op_jsr, 16#1E => op_eor,
    16#2E => op_jsr, 16#3E => op_eor,
    16#4E => op_jsr, 16#5E => op_eor,
    16#6E => op_jsr, 16#7E => op_eor,
    16#8E => op_jsr, 16#9E => op_eor,
    16#AE => op_jsr, 16#BE => op_eor,
    16#CE => op_jsr, 16#DE => op_eor,
    16#EE => op_jsr, 16#FE => op_eor,

    16#0F => op_sth, 16#1F => op_sft,
    16#2F => op_sth, 16#3F => op_sft,
    16#4F => op_sth, 16#5F => op_sft,
    16#6F => op_sth, 16#7F => op_sft,
    16#8F => op_sth, 16#9F => op_sft,
    16#AF => op_sth, 16#BF => op_sft,
    16#CF => op_sth, 16#DF => op_sft,
    16#EF => op_sth, 16#FF => op_sft
}).