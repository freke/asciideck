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

-module(asciideck_to_html).

-export([translate/2]).

translate(AST, Opts) ->
	Output0 = ast(AST),
	Output1 = header_footer(Output0, Opts),
	{CompressExt, Output} = case Opts of
		#{compress := gzip} -> {".gz", zlib:gzip(Output1)};
		_ -> {"", Output1}
	end,
	case Opts of
		#{outdir := Path, outfile := Filename} ->
			file:write_file(binary_to_list(iolist_to_binary(
				[Path, "/", Filename, ".html", CompressExt])), Output);
		#{outdir := Path} ->
			Filename = filename_from_ast(AST),
			file:write_file(binary_to_list(iolist_to_binary(
				[Path, "/", Filename, ".html", CompressExt])), Output);
		_ ->
			Output
	end.

header_footer(Body, _Opts) ->
	[
		"<!DOCTYPE html>\n"
		"<html lang=\"en\">\n"
		"<head>\n"
		"<meta charset=\"utf-8\"/>\n"
		"<title>TODO title</title>\n" %% @todo
		"<link rel=\"stylesheet\" type=\"text/css\" href=\"https://ninenines.eu/css/99s.css?r=1\"/>\n"
		"</head>\n"
		"<body>\n",
		Body,
		"</body>\n"
		"</html>\n"
	].

filename_from_ast([{section_title, #{level := 0}, Filename, _}|_]) ->
	Filename.

%% Loop over all types of AST nodes.

ast(AST) ->
	fold(AST, fun ast_node/1).

fold(AST, Fun) ->
	lists:reverse(lists:foldl(
		fun(Node, Acc) -> [Fun(Node)|Acc] end,
		[], AST)).

ast_node(Node={Type, _, _, _}) ->
	try
		case Type of
			section_title -> section_title(Node);
			paragraph -> paragraph(Node);
			listing_block -> listing_block(Node);
			list -> list(Node);
			table -> table(Node);
			comment_line -> comment_line(Node);
			_ ->
				io:format("Ignored AST node ~p~n", [Node]),
				[]
		end
	catch _:_ ->
		io:format("Ignored AST node ~p~n", [Node]),
		[]
	end.

%% Section titles.

section_title({section_title, #{level := Level}, Title, _}) ->
	LevelC = $1 + Level,
	["<h", LevelC, ">", inline(Title), "</h", LevelC, ">\n"].

%% Paragraphs.

paragraph({paragraph, _, Text, _}) ->
	["<p>", inline(Text), "</p>\n"].

%% Listing blocks.

listing_block({listing_block, Attrs, Listing0, _}) ->
	Listing = case Attrs of
		#{1 := <<"source">>, 2 := _} ->
			try asciideck_source_highlight:filter(Listing0, Attrs) catch C:E -> io:format("~p ~p ~p~n", [C, E, erlang:get_stacktrace()]), exit(bad) end;
		_ ->
			["<pre>", html_encode(Listing0), "</pre>"]
	end,
	[
		"<div class=\"listingblock\">",
		case Attrs of
			#{<<"title">> := Title} ->
				["<div class=\"title\">", inline(Title), "</div>\n"];
			_ ->
				[]
		end,
		"<div class=\"content\">",
		Listing,
		"</div></div>\n"
	].

%% Lists.

list({list, #{type := bulleted}, Items, _}) ->
	["<ul>", fold(Items, fun bulleted_list_item/1), "</ul>\n"];
list({list, #{type := labeled}, Items, _}) ->
	["<dl>", fold(Items, fun labeled_list_item/1), "</dl>\n"].

bulleted_list_item({list_item, _, [{paragraph, _, Text, _}|AST], _}) ->
	[
		"<li>",
		inline(Text), "\n",
		ast(AST),
		"</li>\n"
	].

labeled_list_item({list_item, #{label := Label}, [{paragraph, _, Text, _}|AST], _}) ->
	[
		"<dt>", inline(Label), "</dt>\n",
		"<dd>",
		inline(Text), "\n",
		ast(AST),
		"</dd>\n"
	].

%% Tables.

table({table, _, [{row, _, Head, _}|Rows], _}) ->
	[
		"<table rules=\"all\" width=\"100%\" frame=\"border\" cellspacing=\"0\" cellpadding=\"4\">\n"
		"<thead><tr>", table_head(Head), "</tr></thead>"
		"<tbody>", table_body(Rows), "</tbody>"
		"</table>\n"
	].

table_head(Cells) ->
	[["<th>", inline(Text), "</th>\n"]
		|| {cell, _, Text, _} <- Cells].

table_body(Rows) ->
	[["<tr>", table_body_cells(Cells), "</tr>\n"]
		|| {row, _, Cells, _} <- Rows].

table_body_cells(Cells) ->
	[["<td>", inline(Text), "</td>\n"]
		|| {cell, _, Text, _} <- Cells].

%% Comment lines are printed in the generated file
%% but are not visible in viewers.

comment_line({comment_line, _, Text, _}) ->
	["<!-- ", html_encode(Text), "-->\n"].

%% Inline formatting.

inline(Text) when is_binary(Text) ->
	html_encode(Text);
inline({Link, #{target := Target}, Text, _})
		when Link =:= link; Link =:= xref ->
	["<a href=\"", html_encode(Target), "\">", html_encode(Text), "</a>"];
inline({emphasized, _, Text, _}) ->
	["<em>", inline(Text), "</em>"];
inline({strong, _, Text, _}) ->
	["<strong>", inline(Text), "</strong>"];
inline({inline_literal_passthrough, _, Text, _}) ->
	["<code>", inline(Text), "</code>"];
inline(Text) when is_list(Text) ->
	[inline(T) || T <- Text].

html_encode(Text) ->
	<<case C of
		$& -> <<"&amp;">>;
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		_ -> <<C>>
	end || <<C>> <= Text>>.
