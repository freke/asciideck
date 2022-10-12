%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(asciideck_line_reader).
-behaviour(gen_server).

%% The API is defined in asciideck_reader.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	lines :: [binary()],
	length :: non_neg_integer(),
	pos = 1 :: non_neg_integer(),
	source :: string()
}).

%% API.

-spec start_link([{binary(),map()}]) -> {ok, pid()}.
start_link(Lines) ->
	gen_server:start_link(?MODULE, [Lines], []).

%% gen_server.

init([Lines]) ->
	{_, #{source := Source}} = hd(Lines),

	%% We add an empty line at the end to simplify parsing.
	%% This has the inconvenient that when parsing blocks
	%% this empty line will be included in the result if
	%% the block is not properly closed.
	Lines0 = lists:append(Lines, [{<<>>,#{source=>Source, line=>length(Lines)}}]),
	{ok, #state{lines=Lines0, length=length(Lines0), source=Source}}.

handle_call(read_line, _From, State=#state{length=Length, pos=Pos, source=Source})
		when Pos > Length ->
	{reply, {eof, #{source=>Source, line=>Pos}}, State};
%% @todo I know this isn't the most efficient. We could keep
%% the lines read separately and roll back when set_position
%% wants us to. But it works fine for now.
handle_call(read_line, _From, State=#state{lines=Lines, pos=Pos}) ->
	{reply, lists:nth(Pos, Lines), State#state{pos=Pos + 1}};
handle_call(get_position, _From, State=#state{pos=Pos}) ->
	{reply, Pos, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({set_position, Pos}, State) ->
	{noreply, State#state{pos=Pos}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
