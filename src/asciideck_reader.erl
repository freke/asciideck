%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(asciideck_reader).

-export([read_line/1]).
-export([get_position/1]).
-export([set_position/2]).

-spec read_line(pid()) -> binary() | eof.
read_line(Pid) ->
	gen_server:call(Pid, read_line).

%% @todo peek_line

-spec get_position(pid()) -> pos_integer().
get_position(Pid) ->
	gen_server:call(Pid, get_position).

-spec set_position(pid(), pos_integer()) -> ok.
set_position(Pid, Pos) ->
	gen_server:cast(Pid, {set_position, Pos}).
