%% Copyright (c) 2017-2018, David Åberg <davabe@hotmail.com>
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

%% This pass walks over the tree and parses inline elements.
-module(asciideck_paragraph_pass).

-export([run/1]).

run([]) ->
    [];
run([{paragraph, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
    Data = iolist_to_binary(lists:join("\n", lists:map(fun ({E,_}) -> E end, Items))),
	[{paragraph, Attrs, Data, Ann}|run(Tail)];
run([{listing_block, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
    Data = iolist_to_binary(lists:join("\n", lists:map(fun ({E,_}) when is_binary(E) -> E; (E) when is_binary(E) -> E end, Items))),
	[{listing_block, Attrs, Data, Ann}|run(Tail)];
run([{Type, Attrs, Items, Ann}|Tail]) when is_list(Items) ->
	[{Type, Attrs, run(Items), Ann}|run(Tail)];
run([Para|Tail]) ->
    [Para|run(Tail)].