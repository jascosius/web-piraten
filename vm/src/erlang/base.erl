-module(base).
%-export([print/1,putStr/1,putStrLn/1,getLine/0,getLine/1,show/1]).
-export([print/1,putStr/1,putStrLn/1,show/1]).

% Gibt einen beliebige Datenstruktur aus.
print(X) -> putStrLn(show(X)).

% Gibt einen String aus.
putStr(Str) -> io:fwrite("~s",[Str]).

% Wie putStr, aber zusätzlich wird ein Zeilenumbruch ausgegeben.
putStrLn(Str) -> putStr(Str++"\n").

%%% getLine() liest eine Zeile von der Tastatur ein.
%%getLine() -> getLine("").

%%% getLine(Prompt) liest eine Zeile von der Tastatur ein.
%%% Das Atom P wird als Prompt verwendet.
%%getLine(P) -> L = io:get_line(list_to_atom(P)),
%%	      {Res,_} = lists:split(length(L)-1,L),
%%	      Res.

% show wandelt einen beliebigen Erlang-Wert in einen String um.
% Integer-Listen mit sinnvollen Ascii-Werten werden als String interpretiert.
show(X) when is_atom(X) -> atom_to_list(X);
show(X) when is_integer(X) -> integer_to_list(X);
show(X) when is_pid(X) -> pid_to_list(X);
show(X) when is_tuple(X) -> "{"++showList(tuple_to_list(X))++"}";
show(X) when is_list(X) -> case allChar(X) of
                             true -> "\""++X++"\"";
                             false -> "["++showList(X)++"]"
                           end.

allChar([]) -> true;
allChar([X|Xs]) when is_integer(X) ->
  case ((7<X) and (X<14)) or ((31<X) and (X<127)) of
    true -> allChar(Xs);
    false -> false
  end;
allChar(_) -> false.

showList([]) -> "";
showList([X|Xs]) -> show(X)++case Xs of
                               [] -> "";
                               _ -> ","++showList(Xs)
                             end.
